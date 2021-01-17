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

  begin
    c#send_query
      ~param_types:Postgresql.[|oid_of_ftype INT8; oid_of_ftype INT8|]
      ~params:[|"4100100100"; "5100100100"|]
      "SELECT $1 + $2";
    let r = fetch_single_result c in
    assert (r#status = Tuples_ok);
    assert (r#nfields = 1);
    assert (r#ntuples = 1);
    assert (r#getvalue 0 0 = "9200200200");
  end;

  (* Populate using a prepared statement. *)
  let shown_ntuples = 10 in
  let expected_ntuples = 3 * 100 in
  for i = 0 to 2 do
    let stmt = sprintf "test_ins_%d" i in
    let param_types =
      Array.sub Postgresql.[|oid_of_ftype INT4; oid_of_ftype TEXT|] 0 i
    in
    c#send_prepare stmt ~param_types
      "INSERT INTO postgresql_ocaml_async (a, b) VALUES ($1, $2)";
    assert ((fetch_single_result c)#status = Command_ok);
    for j = 1 to 100 do
      let c0 = string_of_int (i + 3 * j) in
      let c1 = sprintf "The number %d." (i + 3 * j) in
      c#send_query_prepared ~params:[|c0; c1|] stmt;
      assert ((fetch_single_result c)#status = Command_ok)
    done
  done;

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
  assert (r#ntuples = expected_ntuples);
  assert (r#nfields = 3);
  for i = 0 to min r#ntuples shown_ntuples - 1 do
    printf "%s, %s, %s\n" (r#getvalue i 0) (r#getvalue i 1) (r#getvalue i 2)
  done;
  printf "[...]\n";

  (* Run it in single-row mode. *)
  c#send_query_prepared "test_sel";
  c#set_single_row_mode;
  for i = 0 to expected_ntuples do
    match fetch_result c with
    | None -> assert false
    | Some r when i < expected_ntuples ->
      assert (r#status = Single_tuple);
      if i < shown_ntuples then
        printf "%s, %s, %s\n" (r#getvalue 0 0) (r#getvalue 0 1) (r#getvalue 0 2)
    | Some r ->
      assert (r#status = Tuples_ok)
  done;
  printf "[...]\n";
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
