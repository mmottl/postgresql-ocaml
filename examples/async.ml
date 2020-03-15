open Printf
open! Postgresql

let failwith_f fmt = ksprintf failwith fmt

let _ =
  if Array.length Sys.argv <> 2 then (
    Printf.printf "\
      Usage:  async conninfo\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n\
      and run async tests on a temporary table\n";
    exit 1)

let wait_for_result c =
  c#consume_input;
  while c#is_busy do
    ignore (Unix.select [Obj.magic c#socket] [] [] (-.1.0));
    c#consume_input
  done

let fetch_result c = wait_for_result c; c#get_result

let fetch_single_result c =
  match fetch_result c with
  | None -> assert false
  | Some r -> assert (fetch_result c = None); r

(* See http://www.postgresql.org/docs/devel/static/libpq-connect.html *)
let rec finish_conn socket_fd connect_poll = function
  | Polling_failed ->
    printf "f\n%!"
  | Polling_reading ->
    printf "r,%!";
    ignore (Unix.select [socket_fd] [] [] (-1.0));
    finish_conn socket_fd connect_poll (connect_poll ())
  | Polling_writing ->
    printf "w,%!";
    ignore (Unix.select [] [socket_fd] [] (-1.0));
    finish_conn socket_fd connect_poll (connect_poll ())
  | Polling_ok ->
    printf "c\n%!"

let test (c : connection) =
  (* Create a table using a non-prepared statement. *)
  c#send_query "\
    CREATE TEMPORARY TABLE postgresql_ocaml_async \
      (id SERIAL PRIMARY KEY, a INTEGER NOT NULL, b TEXT NOT NULL)";
  assert ((fetch_single_result c)#status = Command_ok);

  (* Create another table which will trigger a notice. *)
  c#send_query "\
    CREATE TEMPORARY TABLE postgresql_ocaml_async_2 \
      (id INTEGER PRIMARY KEY \
        REFERENCES postgresql_ocaml_async ON DELETE CASCADE)";
  assert ((fetch_single_result c)#status = Command_ok);

  (* Populate using a prepared statement. *)
  c#send_prepare "test_ins"
                 "INSERT INTO postgresql_ocaml_async (a, b) VALUES ($1, $2)";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_query_prepared ~params:[|"2"; "two"|] "test_ins";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_query_prepared ~params:[|"3"; "three"|] "test_ins";
  assert ((fetch_single_result c)#status = Command_ok);

  (* Prepare a select statement. *)
  c#send_prepare "test_sel" "SELECT * FROM postgresql_ocaml_async";
  assert ((fetch_single_result c)#status = Command_ok);

  (* Describe it. *)
  c#send_describe_prepared "test_sel";
  let r = fetch_single_result c in
  assert (r#status = Command_ok);
  assert (r#nfields = 3);
  assert (r#fname 0 = "id");
  assert (r#fname 1 = "a");
  assert (r#fname 2 = "b");

  (* Run it. *)
  c#send_query_prepared "test_sel";
  let r = fetch_single_result c in
  assert (r#status = Tuples_ok);
  assert (r#ntuples = 2);
  assert (r#nfields = 3);
  for i = 0 to r#ntuples - 1 do
    Printf.printf "%s %s %s\n"
                  (r#getvalue i 0) (r#getvalue i 1) (r#getvalue i 2)
  done;

  (* Run it in single-row mode. *)
  c#send_query_prepared "test_sel";
  c#set_single_row_mode;
  for i = 0 to 2 do
    match fetch_result c with
    | None -> assert false
    | Some r when i < 2 ->
      assert (r#status = Single_tuple);
      Printf.printf "%s %s %s\n"
                    (r#getvalue 0 0) (r#getvalue 0 1) (r#getvalue 0 2)
    | Some r ->
      assert (r#status = Tuples_ok)
  done;
  assert (fetch_result c = None);

  (* Drop the main table. *)
  c#send_query "DROP TABLE postgresql_ocaml_async CASCADE";
  assert ((fetch_single_result c)#status = Command_ok)

let main () =
  (* Async connect and test. *)
  let c = new connection ~conninfo:Sys.argv.(1) ~startonly:true () in
  finish_conn (Obj.magic c#socket) (fun () -> c#connect_poll) Polling_writing;
  if c#status = Bad then failwith_f "Connection failed: %s" c#error_message;
  assert (c#status = Ok);
  c#set_nonblocking true;
  test c;

  (* Async reset and test again. *)
  if not c#reset_start then failwith_f "reset_start failed: %s" c#error_message;
  finish_conn (Obj.magic c#socket) (fun () -> c#reset_poll) Polling_writing;
  if c#status = Bad then failwith_f "Reset connection bad: %s" c#error_message;
  assert (c#status = Ok);
  c#set_notice_processing `Quiet;
  test c

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
