(* Dump a table to stdout (using the sql command "copy ... to stdout") *)

open! Postgresql

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "\
      Usage:  dump conninfo table\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n\
      and copy [table] to stdout\n";
    exit 1)

let main () =
  let c = new connection ~conninfo:Sys.argv.(1) () in
  let _ = c#exec ~expect:[Copy_out] ("copy " ^ Sys.argv.(2) ^ " to stdout") in
  c#copy_out_channel stdout;
  c#finish

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
