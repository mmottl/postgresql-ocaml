module C = Configurator.V1
open Printf

let find_number ~pos str =
  let len = String.length str in
  let rec skip_other ~pos =
    if pos = len then pos
    else
      match str.[pos] with '0' .. '9' -> pos | _ -> skip_other ~pos:(pos + 1)
  in
  let pos = skip_other ~pos in
  let rec loop ~pos =
    if pos = len then ([], pos)
    else
      match str.[pos] with
      | '0' .. '9' as c ->
          let acc, next = loop ~pos:(pos + 1) in
          (String.make 1 c :: acc, next)
      | _ -> ([], pos)
  in
  let number_lst, next = loop ~pos in
  (String.concat "" number_lst, next)

let pg_major_minor ic =
  let line = input_line ic in
  let print_fail () = eprintf "Unable to find versions from line '%s'" line in
  let exit_fail () =
    print_fail ();
    exit 1
  in
  try
    let first_space = String.index line ' ' in
    let major, next = find_number ~pos:first_space line in
    let minor =
      (* Can also handle release candidates *)
      let c = line.[next] in
      if c = '.' then fst (find_number ~pos:next line)
      else if c <> 'r' || line.[next + 1] <> 'c' then exit_fail ()
      else "0"
    in
    if major = "" || minor = "" then exit_fail ()
    else
      ("-DPG_OCAML_MAJOR_VERSION=" ^ major, "-DPG_OCAML_MINOR_VERSION=" ^ minor)
  with exn ->
    print_fail ();
    raise exn

let major_minor_from_pgconfig () =
  let cmd = "pg_config --version" in
  let ic =
    try Unix.open_process_in cmd
    with exc ->
      eprintf "could not open pg_config, cmd: '%s'" cmd;
      raise exc
  in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> pg_major_minor ic)

let keep_digits =
  let rec aux version_line len i =
    if i < len then
      match version_line.[i] with
      | '0' .. '9' | '.' -> aux version_line len (i + 1)
      | _ -> i
    else i
  in
  fun version_line ->
    let len = String.length version_line in
    let cut = aux version_line len 0 in
    String.sub version_line 0 cut

let major_minor_from_pkg_config () =
  let bin = Sys.getenv_opt "PKG_CONFIG" |> Option.value ~default:"pkg-config" in
  let args = Sys.getenv_opt "PKG_CONFIG_ARGN" |> Option.value ~default:"" in
  let cmd = Format.asprintf "%s %s --modversion libpq" bin args in
  let ic = Unix.open_process_in cmd in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let version_line = input_line ic in
  match String.split_on_char '.' (keep_digits version_line) with
  | [ major ] ->
      ("-DPG_OCAML_MAJOR_VERSION=" ^ major, "-DPG_OCAML_MINOR_VERSION=0")
  | major :: minor :: _ ->
      ("-DPG_OCAML_MAJOR_VERSION=" ^ major, "-DPG_OCAML_MINOR_VERSION=" ^ minor)
  | _ ->
      eprintf "Unable to parse libpq version: %s" version_line;
      exit 1

let from_pgconfig () =
  let cmd = "pg_config --includedir --libdir --version" in
  let ic =
    try Unix.open_process_in cmd
    with exc ->
      eprintf "could not open pg_config, cmd: '%s'" cmd;
      raise exc
  in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let pgsql_includedir = "-I" ^ input_line ic in
      let pgsql_libdir = "-L" ^ input_line ic in
      let major, minor = pg_major_minor ic in
      {
        C.Pkg_config.cflags = [ pgsql_includedir; major; minor ];
        libs = [ pgsql_libdir; "-lpq" ];
      })

let () =
  C.main ~name:"postgresql" (fun c ->
      let conf =
        match C.Pkg_config.get c with
        | Some pc -> (
            match C.Pkg_config.query pc ~package:"libpq" with
            | Some conf ->
                let major, minor = major_minor_from_pkg_config () in
                {
                  conf with
                  C.Pkg_config.cflags = major :: minor :: conf.cflags;
                }
            | None -> { C.Pkg_config.cflags = []; libs = [] })
        | None -> from_pgconfig ()
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
