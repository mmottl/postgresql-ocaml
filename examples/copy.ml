open Postgresql
open Printf

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "\
      Usage:  copy conninfo\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n";
    exit 1
  end

let create_sql = "\
  CREATE TEMPORARY TABLE pgo_copy_test (\
    id SERIAL PRIMARY KEY, \
    name text UNIQUE NOT NULL, \
    x integer NOT NULL\
  )"

let populate ?error_msg (c : connection) =
  let _ = c#exec ~expect:[Copy_in] "COPY pgo_copy_test (name, x) FROM STDIN" in
  for i = 0 to 9999 do
    match c#put_copy_data (sprintf "c%d\t%d\n" i i) with
    | Put_copy_queued -> ()
    | Put_copy_error | Put_copy_not_queued -> assert false
  done;
  begin match c#put_copy_end ?error_msg () with
  | Put_copy_queued -> ()
  | Put_copy_error | Put_copy_not_queued -> assert false
  end;
  match c#get_result with
  | Some result ->
      begin match error_msg, result#status with
      | None, Command_ok -> ()
      | Some _msg, Fatal_error -> ()
      | _ -> assert false end
  | None -> assert false

let verify (c : connection) =
  let _res =
    c#exec ~expect:[Copy_out] "COPY pgo_copy_test (id, name, x) TO STDOUT"
  in
  for i = 0 to 9999 do
    match c#get_copy_data () with
    | Get_copy_data data ->
        assert (data = sprintf "%d\tc%d\t%d\n" (i + 1) i i)
    | Get_copy_wait | Get_copy_end | Get_copy_error -> assert false
  done;
  match c#get_copy_data () with
  | Get_copy_end -> ()
  | _ -> assert false

let main () =
  let c = new connection ~conninfo:Sys.argv.(1) () in
  let _ = c#exec ~expect:[Command_ok] create_sql in
  populate c;
  populate c ~error_msg:"test failure";
  verify c;
  c#finish

let () =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
