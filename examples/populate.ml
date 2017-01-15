(* Inverse operation of "dump" (using the sql command "copy ... from stdin") *)

open! Postgresql

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "\
      Usage:  populate conninfo table\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n\
      and copy stdin to [table]\n";
    exit 1)

let main () =
  let c = new connection ~conninfo:Sys.argv.(1) () in
  let _ = c#exec ~expect:[Copy_in] ("copy " ^ Sys.argv.(2) ^ " from stdin") in
  c#copy_in_channel stdin;
  c#finish

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
