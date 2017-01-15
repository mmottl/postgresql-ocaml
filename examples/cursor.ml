(* Dump a table to stdout (using a cursor for demonstration) *)

open! Postgresql

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "\
      Usage:  cursor conninfo table\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n\
      and copy [table] to stdout using a cursor\n";
    exit 1)

let main () =
  let c = new connection ~conninfo:Sys.argv.(1) () in
  ignore (c#exec ~expect:[Command_ok] "BEGIN");
  ignore (
    c#exec
      ~expect:[Command_ok]
      ("DECLARE my_cursor CURSOR FOR SELECT * FROM " ^ Sys.argv.(2)));
  let rec loop () =
    let res = c#exec ~expect:[Tuples_ok] "FETCH IN my_cursor" in
    if res#ntuples <> 0 then (
      let tpl = res#get_tuple 0 in
      print_string tpl.(0);
      for i = 1 to Array.length tpl - 1 do print_string (" " ^ tpl.(i)) done;
      print_newline ();
      loop ()) in
  loop ();
  ignore (c#exec ~expect:[Command_ok] "CLOSE my_cursor");
  ignore (c#exec ~expect:[Command_ok] "END");
  c#finish

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
