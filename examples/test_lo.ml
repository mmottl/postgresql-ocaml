(* A simple demonstration of Large Objects manipulation *)

open! Postgresql

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage:  %s conninfo\n" Sys.argv.(0);
    exit 1)

let conninfo = Sys.argv.(1)

let main () =
  let c = new connection ~conninfo () in
  let _ = c#exec ~expect:[Command_ok] "begin" in
  let oid = c#lo_creat in
  let lo = c#lo_open oid in
  c#lo_write "Hello world !\n" lo;
  c#lo_close lo;
  c#lo_export oid "/dev/stdout";
  let _ = c#exec ~expect:[Command_ok] "end" in
  c#finish

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
