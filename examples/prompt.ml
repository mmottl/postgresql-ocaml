(* A simple replacement for psql *)

open Printf
open! Postgresql

let _ =
  if Array.length Sys.argv <> 2 then (
    eprintf "\n Usage:  %s conninfo\n" Sys.argv.(0);
    exit 1)

let conninfo = Sys.argv.(1)

let print_conn_info conn =
  printf "db      = %s\n" conn#db;
  printf "user    = %s\n" conn#user;
  printf "pass    = %s\n" conn#pass;
  printf "host    = %s\n" conn#host;
  printf "port    = %s\n" conn#port;
  printf "tty     = %s\n" conn#tty;
  printf "option  = %s\n" conn#options;
  printf "pid     = %i\n" conn#backend_pid

let print_res conn res =
  match res#status with
  | Empty_query -> printf "Empty query\n"
  | Command_ok -> printf "Command ok [%s]\n" res#cmd_status
  | Tuples_ok ->
      printf "Tuples ok\n";
      printf "%i tuples with %i fields\n" res#ntuples res#nfields;
      print_endline (String.concat ";" res#get_fnames_lst);
      for tuple = 0 to res#ntuples - 1 do
        for field = 0 to res#nfields - 1  do
          printf "%s, " (res#getvalue tuple field)
        done;
        print_newline ()
      done
  | Copy_out -> printf "Copy out:\n"; conn#copy_out print_endline
  | Copy_in -> printf "Copy in, not handled!\n"; exit 1
  | Bad_response -> printf "Bad response: %s\n" res#error; conn#reset
  | Nonfatal_error -> printf "Non fatal error: %s\n" res#error
  | Fatal_error -> printf "Fatal error: %s\n" res#error
  | Copy_both -> printf "Copy in/out, not handled!\n"; exit 1
  | Single_tuple -> printf "Single tuple, not handled!\n"; exit 1

let rec dump_res conn =
  match conn#get_result with
  | Some res -> print_res conn res; flush stdout; dump_res conn
  | None -> ()

let rec dump_notification conn =
  match conn#notifies with
  | Some { Notification.name; pid; extra } ->
      printf "Notication from backend %i: [%s] [%s]\n" pid name extra;
      flush stdout;
      dump_notification conn
  | None -> ()

let listener conn =
  try
    while true do
      let socket : Unix.file_descr = Obj.magic conn#socket in
      let _ = Unix.select [socket] [] [] 1. in
      conn#consume_input;
      dump_notification conn
    done
  with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)

let main () =
  if Obj.is_block (Obj.repr Unix.stdin) then
    failwith "cannot run on Windows";
  let conn = new connection ~conninfo () in
  print_conn_info conn;
  flush stdout;
  conn#set_notice_processor (fun s -> eprintf "postgresql error [%s]\n" s);
  let _ = Thread.create listener conn in
  try
    while true do
      print_string "> ";
      let s = read_line () in
      conn#send_query s;
      dump_res conn
    done
  with End_of_file -> conn#finish

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
