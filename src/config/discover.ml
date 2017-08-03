open Base
open Stdio

let () =
  let module C = Configurator in
  C.main ~name:"postgresql" (fun _c ->
    let cmd = "pg_config --includedir --libdir --version" in
    let ic =
      try Unix.open_process_in cmd
      with exc -> eprintf "could not open pg_config, cmd: '%s'" cmd; raise exc
    in
    Exn.protectx ic ~finally:In_channel.close ~f:(fun ic ->
      let pgsql_includedir = "-I" ^ In_channel.input_line_exn ic in
      let pgsql_libdir = "-L" ^ In_channel.input_line_exn ic in
      let major, minor =
        let line = In_channel.input_line_exn ic in
        let print_fail () =
          eprintf "Unable to find versions from line '%s', cmd: '%s'" line cmd
        in
        try
          let first_space = String.index_exn line ' ' in
          let first_dot = String.index_exn line '.' in
          let first_part =
            let len = first_dot - first_space - 1 in
            String.sub line ~pos:(first_space + 1) ~len
          in
          let second_part =
            let len = String.length line - first_dot - 1 in
            String.sub line ~pos:(first_dot + 1) ~len
          in
          let search_version s =
            let version = ref "" in
            let stop = ref false in
            let check_car c =
              let ascii = Char.to_int c in
              if (ascii >= 48 && ascii <= 57 && not !stop) then
                version := !version ^ (String.make 1 c)
              else stop := true
            in
            let () = String.iter ~f:check_car s in
            !version
          in
          let major = search_version first_part in
          let minor = search_version second_part in
          if String.(major <> "" && minor <> "") then
            "-DPG_OCAML_MAJOR_VERSION=" ^ major,
            "-DPG_OCAML_MINOR_VERSION=" ^ minor
          else begin
            print_fail ();
            Caml.exit 1
          end
        with exn -> print_fail (); raise exn
      in
      let conf = {
        C.Pkg_config.
        cflags = [pgsql_includedir; major; minor];
        libs = [pgsql_libdir; "-lpq"];
      } in
      let write_sexp file sexp =
        Out_channel.write_all file ~data:(Sexp.to_string sexp)
      in
      write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp"
        (sexp_of_list sexp_of_string conf.libs)))
