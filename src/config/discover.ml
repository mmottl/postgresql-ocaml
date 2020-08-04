open Printf

let find_number ~pos str =
  let len = String.length str in
  let rec skip_other ~pos =
    if pos = len then pos
    else
      match str.[pos] with
      | '0'..'9' -> pos
      | _ -> skip_other ~pos:(pos + 1)
  in
  let pos = skip_other ~pos in
  let rec loop ~pos =
    if pos = len then [], pos
    else match str.[pos] with
    | '0'..'9' as c ->
        let acc, next = loop ~pos:(pos + 1) in
        String.make 1 c :: acc, next
    | _ -> [], pos
  in
  let number_lst, next = loop ~pos in
  String.concat "" number_lst, next

let () =
  let module C = Configurator.V1 in
  C.main ~name:"postgresql" (fun _c ->
    let cmd = "pg_config --includedir --libdir --version" in
    let ic =
      try Unix.open_process_in cmd
      with exc -> eprintf "could not open pg_config, cmd: '%s'" cmd; raise exc
    in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let pgsql_includedir = "-I" ^ input_line ic in
      let pgsql_libdir = "-L" ^ input_line ic in
      let major, minor =
        let line = input_line ic in
        let print_fail () =
          eprintf "Unable to find versions from line '%s', cmd: '%s'" line cmd
        in
        let exit_fail () = print_fail (); exit 1 in
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
            "-DPG_OCAML_MAJOR_VERSION=" ^ major,
            "-DPG_OCAML_MINOR_VERSION=" ^ minor
        with exn -> print_fail (); raise exn
      in
      let conf = {
        C.Pkg_config.
        cflags = [pgsql_includedir; major; minor];
        libs = [pgsql_libdir; "-lpq"];
      } in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs))
