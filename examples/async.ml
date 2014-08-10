open Postgresql

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

let main () =
  let c = new connection ~conninfo:Sys.argv.(1) () in
  c#set_nonblocking true;
  c#send_query "\
    CREATE TEMPORARY TABLE postgresql_ocaml_async \
      (id SERIAL PRIMARY KEY, a INTEGER NOT NULL, b TEXT NOT NULL)";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_prepare "test_ins"
                 "INSERT INTO postgresql_ocaml_async (a, b) VALUES ($1, $2)";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_query_prepared ~params:[|"2"; "two"|] "test_ins";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_query_prepared ~params:[|"3"; "three"|] "test_ins";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_prepare "test_sel" "SELECT * FROM postgresql_ocaml_async";
  assert ((fetch_single_result c)#status = Command_ok);
  c#send_describe_prepared "test_sel";
  let r = fetch_single_result c in
  assert (r#status = Command_ok);
  assert (r#nfields = 3);
  assert (r#fname 0 = "id");
  assert (r#fname 1 = "a");
  assert (r#fname 2 = "b");
  c#send_query_prepared "test_sel";
  let r = fetch_single_result c in
  assert (r#status = Tuples_ok);
  assert (r#ntuples = 2);
  assert (r#nfields = 3);
  for i = 0 to r#ntuples - 1 do
    Printf.printf "%s %s %s\n"
                  (r#getvalue i 0) (r#getvalue i 1) (r#getvalue i 2)
  done

let _ =
  try main () with
  | Error e -> prerr_endline (string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)
