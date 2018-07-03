(* Create a table, insert a binary string various ways, and read it various ways *)

open! Postgresql

let _ =
  if Array.length Sys.argv <> 3 then (
    Printf.printf "\
      Usage:  dump conninfo table\n\
      Connect to PostgreSQL with [conninfo] (e.g. \"host=localhost\"),\n\
      and create, write to, read from, and then DELETE [table]\n";
    exit 1)

let table = Sys.argv.(2)
let c =
  try new connection ~conninfo:Sys.argv.(1) () with
  | Error e ->
    prerr_endline (string_of_error e);
    exit 34
  | e ->
    prerr_endline (Printexc.to_string e);
    exit 35


let create_table () : unit =
  c#exec ~expect:[Command_ok]
    ("create table " ^ table ^ " (data bytea)")
  |> ignore

let write_escaped data : unit =
  c#exec ~expect:[Command_ok]
    ("insert into " ^ table
     ^ " (data)"
     ^ " VALUES ('"
     ^ c#escape_bytea data
     ^ "')")
  |> ignore

let write_binary data : unit =
  c#exec ~expect:[Command_ok]
    ~params:[|data|] ~binary_params:[|true|]
    ("insert into " ^ table
     ^ " (data)"
     ^ " VALUES ($1)")
  |> ignore

let read_escaped () : string list =
  let result =
    c#exec ~expect:[Tuples_ok]
      ("select data from " ^ table)
  in
  result#get_all_lst
  |> List.map List.hd
  |> List.map unescape_bytea

let read_binary () : string list =
  let result =
    c#exec ~expect:[Tuples_ok] ~binary_result:true
      ("select data from " ^ table)
  in
  result#get_all_lst
  |> List.map List.hd

let delete_table () : unit =
  c#exec ~expect:[Command_ok]
    ("drop table if exists " ^ table ^ " cascade")
  |> ignore

let string_list_to_string  l =
  "["
  ^ (String.concat ", " l)
  ^ "]"

let main () =
  delete_table ();
  create_table ();
  (* first 'line' of a compiled binary, pasted in *)
  let data = "ELF>0L@–„$@8	@%$@@@¯¯888†¿†¿ p√p√0p√0ÿ‚! (ƒ(ƒ0(ƒ000TTTDDPÂtd`‘`‘`‘Ã[Ã[QÂtdRÂtdp√p√0p√0êê∑Åù[»·≥¢l∆'„¶•0ë5!@ÇÄ%$ √ bFÄ@ê Ú0êÄàê‡ÄÉê– " in
  write_escaped data;
  write_binary data;
  let read_esc = read_escaped () in
  let read_bin = read_binary () in
  let expected = [data; data] in
  (* print_endline ("expected: " ^ string_list_to_string expected); *)
  (* print_endline ("read_escaped: " ^ string_list_to_string read_esc); *)
  (* print_endline ("read_binary: " ^ string_list_to_string read_bin); *)
  if read_esc <> expected
  then prerr_endline "read_escaped <> expected";
  if read_bin <> expected
  then prerr_endline "read_binary <> expected";
  if read_esc <> read_bin
  then prerr_endline "read_escaped <> read_binary";
  delete_table ();
  ()


let _ =
  try main () with
  | Error e ->
    prerr_endline (string_of_error e);
    exit 23
  | e ->
    prerr_endline (Printexc.to_string e);
    exit 24
