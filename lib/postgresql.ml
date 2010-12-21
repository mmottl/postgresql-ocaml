(*
   PostgreSQL-OCAML - OCaml-interface to the PostgreSQL database

   Copyright (C) 2004-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   Copyright (C) 2001  Alain Frisch  (version: postgres-20010808)
   email: Alain.Frisch@ens.fr
   WWW:   http://www.eleves.ens.fr/home/frisch

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf

type oid = int
type large_object = int

exception Oid of oid

let invalid_oid = 0

module FFormat = struct
  type t =
    | TEXT
    | BINARY
end

type ftype =
  | BOOL
  | BYTEA
  | CHAR
  | NAME
  | INT8
  | INT2
  | INT2VECTOR
  | INT4
  | REGPROC
  | TEXT
  | OID
  | TID
  | XID
  | CID
  | OIDVECTOR
  | POINT
  | LSEG
  | PATH
  | BOX
  | POLYGON
  | LINE
  | FLOAT4
  | FLOAT8
  | ABSTIME
  | RELTIME
  | TINTERVAL
  | UNKNOWN
  | CIRCLE
  | CASH
  | MACADDR
  | INET
  | CIDR
  | ACLITEM
  | BPCHAR
  | VARCHAR
  | DATE
  | TIME
  | TIMESTAMP
  | TIMESTAMPTZ
  | INTERVAL
  | TIMETZ
  | BIT
  | VARBIT
  | NUMERIC
  | REFCURSOR
  | REGPROCEDURE
  | REGOPER
  | REGOPERATOR
  | REGCLASS
  | REGTYPE
  | RECORD
  | CSTRING
  | ANY
  | ANYARRAY
  | VOID
  | TRIGGER
  | LANGUAGE_HANDLER
  | INTERNAL
  | OPAQUE
  | ANYELEMENT

external ftype_of_oid : oid -> ftype = "ftype_of_oid_stub"
external oid_of_ftype : ftype -> oid = "oid_of_ftype_stub"

let string_of_ftype = function
  | BOOL -> "BOOL"
  | BYTEA -> "BYTEA"
  | CHAR -> "CHAR"
  | NAME -> "NAME"
  | INT8 -> "INT8"
  | INT2 -> "INT2"
  | INT2VECTOR -> "INT2VECTOR"
  | INT4 -> "INT4"
  | REGPROC -> "REGPROC"
  | TEXT -> "TEXT"
  | OID -> "OID"
  | TID -> "TID"
  | XID -> "XID"
  | CID -> "CID"
  | OIDVECTOR -> "OIDVECTOR"
  | POINT -> "POINT"
  | LSEG -> "LSEG"
  | PATH -> "PATH"
  | BOX -> "BOX"
  | POLYGON -> "POLYGON"
  | LINE -> "LINE"
  | FLOAT4 -> "FLOAT4"
  | FLOAT8 -> "FLOAT8"
  | ABSTIME -> "ABSTIME"
  | RELTIME -> "RELTIME"
  | TINTERVAL -> "TINTERVAL"
  | UNKNOWN -> "UNKNOWN"
  | CIRCLE -> "CIRCLE"
  | CASH -> "CASH"
  | MACADDR -> "MACADDR"
  | INET -> "INET"
  | CIDR -> "CIDR"
  | ACLITEM -> "ACLITEM"
  | BPCHAR -> "BPCHAR"
  | VARCHAR -> "VARCHAR"
  | DATE -> "DATE"
  | TIME -> "TIME"
  | TIMESTAMP -> "TIMESTAMP"
  | TIMESTAMPTZ -> "TIMESTAMPTZ"
  | INTERVAL -> "INTERVAL"
  | TIMETZ -> "TIMETZ"
  | BIT -> "BIT"
  | VARBIT -> "VARBIT"
  | NUMERIC -> "NUMERIC"
  | REFCURSOR -> "REFCURSOR"
  | REGPROCEDURE -> "REGPROCEDURE"
  | REGOPER -> "REGOPER"
  | REGOPERATOR -> "REGOPERATOR"
  | REGCLASS -> "REGCLASS"
  | REGTYPE -> "REGTYPE"
  | RECORD -> "RECORD"
  | CSTRING -> "CSTRING"
  | ANY -> "ANY"
  | ANYARRAY -> "ANYARRAY"
  | VOID -> "VOID"
  | TRIGGER -> "TRIGGER"
  | LANGUAGE_HANDLER -> "LANGUAGE_HANDLER"
  | INTERNAL -> "INTERNAL"
  | OPAQUE -> "OPAQUE"
  | ANYELEMENT -> "ANYELEMENT"

let ftype_of_string = function
  | "BOOL" -> BOOL
  | "BYTEA" -> BYTEA
  | "CHAR" -> CHAR
  | "NAME" -> NAME
  | "INT8" -> INT8
  | "INT2" -> INT2
  | "INT2VECTOR" -> INT2VECTOR
  | "INT4" -> INT4
  | "REGPROC" -> REGPROC
  | "TEXT" -> TEXT
  | "OID" -> OID
  | "TID" -> TID
  | "XID" -> XID
  | "CID" -> CID
  | "OIDVECTOR" -> OIDVECTOR
  | "POINT" -> POINT
  | "LSEG" -> LSEG
  | "PATH" -> PATH
  | "BOX" -> BOX
  | "POLYGON" -> POLYGON
  | "LINE" -> LINE
  | "FLOAT4" -> FLOAT4
  | "FLOAT8" -> FLOAT8
  | "ABSTIME" -> ABSTIME
  | "RELTIME" -> RELTIME
  | "TINTERVAL" -> TINTERVAL
  | "UNKNOWN" -> UNKNOWN
  | "CIRCLE" -> CIRCLE
  | "CASH" -> CASH
  | "MACADDR" -> MACADDR
  | "INET" -> INET
  | "CIDR" -> CIDR
  | "ACLITEM" -> ACLITEM
  | "BPCHAR" -> BPCHAR
  | "VARCHAR" -> VARCHAR
  | "DATE" -> DATE
  | "TIME" -> TIME
  | "TIMESTAMP" -> TIMESTAMP
  | "TIMESTAMPTZ" -> TIMESTAMPTZ
  | "INTERVAL" -> INTERVAL
  | "TIMETZ" -> TIMETZ
  | "BIT" -> BIT
  | "VARBIT" -> VARBIT
  | "NUMERIC" -> NUMERIC
  | "REFCURSOR" -> REFCURSOR
  | "REGPROCEDURE" -> REGPROCEDURE
  | "REGOPER" -> REGOPER
  | "REGOPERATOR" -> REGOPERATOR
  | "REGCLASS" -> REGCLASS
  | "REGTYPE" -> REGTYPE
  | "RECORD" -> RECORD
  | "CSTRING" -> CSTRING
  | "ANY" -> ANY
  | "ANYARRAY" -> ANYARRAY
  | "VOID" -> VOID
  | "TRIGGER" -> TRIGGER
  | "LANGUAGE_HANDLER" -> LANGUAGE_HANDLER
  | "INTERNAL" -> INTERNAL
  | "OPAQUE" -> OPAQUE
  | "ANYELEMENT" -> ANYELEMENT
  | str -> failwith ("ftype_of_string: unknown ftype: " ^ str)

external init : unit -> unit = "PQocaml_init"

let null = ""

let () =
  Callback.register_exception "Postgresql.Oid" (Oid invalid_oid);
  Callback.register "Postgresql.null" null;
  init ()

type connection_status = Ok | Bad

type conninfo_option =
  {
    cio_keyword : string;
    cio_envvar : string;
    cio_compiled : string option;
    cio_val : string option;
    cio_label : string;
    cio_dispchar : string;
    cio_dispsize : int;
  }

type result_status =
  | Empty_query
  | Command_ok
  | Tuples_ok
  | Copy_out
  | Copy_in
  | Bad_response
  | Nonfatal_error
  | Fatal_error

external result_status : result_status -> string = "PQresStatus_stub"

type getline_result = EOF | LineRead | BufFull

type getline_async_result =
  | EndOfData
  | NoData
  | DataRead of int
  | PartDataRead of int

type seek_cmd =
  | SEEK_SET
  | SEEK_CUR
  | SEEK_END

type error =
  | Field_out_of_range of int * int
  | Tuple_out_of_range of int * int
  | Binary
  | Connection_failure of string
  | Unexpected_status of result_status * string * (result_status list)
  | Cancel_failure of string

let string_of_error = function
  | Field_out_of_range (i, n) ->
      sprintf "Field number %i is out of range [0..%i]" i (n - 1)
  | Tuple_out_of_range (i, n) ->
      sprintf "Tuple number %i is out of range [0..%i]" i (n - 1)
  | Binary -> sprintf "This function does not accept binary tuples"
  | Connection_failure s -> "Connection failure: " ^ s
  | Unexpected_status (s, msg, sl) ->
      sprintf "Result status %s unexpected (expected status:%s); %s"
        (result_status s) (String.concat "," (List.map result_status sl))
        msg
  | Cancel_failure s -> "Cancel failure: " ^ s

exception Error of error


module Stub = struct
  (* Database Connection Functions *)

  type connection
  type result

  external conn_isnull : connection -> bool = "PQconn_isnull" "noalloc"
  external connect : string -> connection = "PQconnectdb_stub"
  external finish : connection -> unit = "PQfinish_stub"
  external reset : connection -> unit = "PQreset_stub"

  external db : connection -> string = "PQdb_stub"
  external user : connection -> string = "PQuser_stub"
  external pass : connection -> string = "PQpass_stub"
  external host : connection -> string = "PQhost_stub"
  external port : connection -> string = "PQport_stub"
  external tty : connection -> string = "PQtty_stub"
  external options : connection -> string = "PQoptions_stub"

  external connection_status :
    connection -> connection_status = "PQstatus_stub" "noalloc"

  external error_message : connection -> string = "PQerrorMessage_stub"
  external backend_pid : connection -> int = "PQbackendPID_stub" "noalloc"


  (* Command Execution Functions *)

  external result_isnull : result -> bool = "PQres_isnull" "noalloc"

  external exec_params :
    connection -> string -> string array -> bool array -> result
    = "PQexecParams_stub"

  external describe_prepared :
    connection -> string -> result = "PQdescribePrepared_stub"

  external result_status :
    result -> result_status = "PQresultStatus_stub" "noalloc"

  external result_error : result -> string = "PQresultErrorMessage_stub"

  external make_empty_res :
    connection -> result_status -> result = "PQmakeEmptyPGresult_stub"

  external ntuples : result -> int = "PQntuples_stub" "noalloc"

(* FIXME: switch to noalloc once PostgreSQL 8.2 is out for CentOS *)
(*   external nparams : result -> int = "PQnparams_stub" "noalloc" *)
  external nparams : result -> int = "PQnparams_stub"

  external nfields : result -> int = "PQnfields_stub" "noalloc"
  external fname : result -> int -> string = "PQfname_stub"
  external fnumber : result -> string -> int ="PQfnumber_stub" "noalloc"
  external fformat : result -> int -> FFormat.t = "PQfformat_stub" "noalloc"
  external ftype : result -> int -> oid = "PQftype_stub" "noalloc"

(* FIXME: switch to noalloc once PostgreSQL 8.2 is out for CentOS *)
(*   external paramtype : result -> int -> oid = "PQparamtype_stub" "noalloc" *)
  external paramtype : result -> int -> oid = "PQparamtype_stub"

  external fmod : result -> int -> int = "PQfmod_stub" "noalloc"
  external fsize : result -> int -> int = "PQfsize_stub" "noalloc"
  external binary_tuples : result -> bool = "PQbinaryTuples_stub" "noalloc"

  external getvalue : result -> int -> int -> string = "PQgetvalue_stub"

  external getisnull :
    result -> int -> int -> bool = "PQgetisnull_stub" "noalloc"

  external getlength :
    result -> int -> int -> int = "PQgetlength_stub" "noalloc"

  external cmd_status : result -> string = "PQcmdStatus_stub"
  external cmd_tuples : result -> string = "PQcmdTuples_stub"
  external oid_value : result -> oid = "PQoidValue_stub" "noalloc"


  (* Asynchronous Query Processing *)

  external set_nonblocking :
    connection -> bool -> int = "PQsetnonblocking_stub" "noalloc"

  external is_nonblocking :
    connection -> bool = "PQisnonblocking_stub" "noalloc"

  external send_query_params :
    connection -> string -> string array -> bool array -> int
    = "PQsendQueryParams_stub" "noalloc"

  external get_result : connection -> result = "PQgetResult_stub"
  external consume_input : connection -> int = "PQconsumeInput_stub" "noalloc"
  external is_busy : connection -> bool = "PQisBusy_stub" "noalloc"
  external flush : connection -> int = "PQflush_stub" "noalloc"
  external socket : connection -> int = "PQsocket_stub" "noalloc"
  external request_cancel : connection -> string option = "PQCancel_stub"


  (* Asynchronous Notification *)

  external notifies : connection -> (string * int) option = "PQnotifies_stub"


  (* Functions Associated with the COPY Command *)

  external getline :
    connection -> string -> int -> int -> int = "PQgetline_stub"

  external getline_async :
    connection -> string -> int -> int -> int = "PQgetlineAsync_stub" "noalloc"

  external putline : connection -> string -> int = "PQputline_stub"

  external putnbytes :
    connection -> string -> int -> int -> int = "PQputnbytes_stub"

  external endcopy : connection -> int = "PQendcopy_stub"

  external escape_string_conn :
    connection -> string -> pos : int -> len : int -> string
    = "PQescapeStringConn_stub"

  external escape_bytea_conn :
    connection -> string -> pos : int -> len : int -> string
    = "PQescapeByteaConn_stub"


  (* Control Functions *)

  external set_notice_processor :
    connection -> (string -> unit) -> unit = "PQsetNoticeProcessor_stub"


  (* Large objects *)

  external lo_creat : connection -> oid = "lo_creat_stub"
  external lo_import : connection -> string -> oid = "lo_import_stub"
  external lo_export : connection -> oid -> string -> int = "lo_export_stub"
  external lo_open : connection -> oid -> large_object = "lo_open_stub"
  external lo_close : connection -> large_object -> int = "lo_close_stub"
  external lo_tell : connection -> large_object -> int = "lo_tell_stub"
  external lo_unlink : connection -> oid -> oid = "lo_unlink_stub"

  external lo_read :
    connection -> large_object -> string -> int -> int -> int = "lo_read_stub"

  external lo_write :
    connection -> large_object -> string -> int -> int -> int = "lo_write_stub"

  external lo_seek :
    connection -> large_object -> int -> seek_cmd -> int = "lo_lseek_stub"
end


(* Escaping *)

external unescape_bytea_stub : string -> string = "PQunescapeBytea_stub"

let unhexdigit = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> assert false

let unescape_bytea s =
  if String.length s >= 2 && s.[0] = '\\' && s.[1] = 'x' then
    (* This is Postgresql's 9.0 new hex format for encoding bytea.
       This format is not recognized by older client, but sent by
       default by the server. *)
    let len = (String.length s - 2) / 2 in
    let r = String.create len in
    for i = 0 to len - 1 do
      let pos = 2 + i * 2 in
      r.[i] <- Char.chr (16 * unhexdigit s.[pos] + unhexdigit s.[pos + 1])
    done;
    r
  else
    unescape_bytea_stub s

(* Query results *)

class result res =
  let nfields = Stub.nfields res in
  let ntuples = Stub.ntuples res in
(* FIXME: get rid of laziness once PostgreSQL 8.2 is out for CentOS *)
(*   let nparams = Stub.nparams res in *)
  let nparams = lazy (Stub.nparams res) in
  let binary_tuples = Stub.binary_tuples res in
  let check_field field =
    if field < 0 || field >= nfields then
      raise (Error (Field_out_of_range (field, nfields))) in
  let check_param param =
    let nparams = Lazy.force nparams in
    if param < 0 || param >= nparams then
      raise (Error (Field_out_of_range (param, nparams))) in
  let check_tuple tuple =
    if tuple < 0 || tuple >= ntuples then
      raise (Error (Tuple_out_of_range (tuple, ntuples))) in
object
  method status = Stub.result_status res
  method error = Stub.result_error res
  method ntuples = ntuples
  method nparams = Lazy.force nparams
  method nfields = nfields
  method binary_tuples = binary_tuples
  method fname field = check_field field; Stub.fname res field

  method fnumber s =
    let n = Stub.fnumber res s in
    if n = -1 then raise Not_found else n

  method fformat field =
    check_field field;
    Stub.fformat res field

  method ftype field =
    check_field field;
    ftype_of_oid (Stub.ftype res field)

  method paramtype field =
    check_param field;
    ftype_of_oid (Stub.paramtype res field)

  method fmod field = check_field field; Stub.fmod res field
  method fsize field = check_field field; Stub.fsize res field

  method getvalue tuple field =
    check_field field;
    check_tuple tuple;
    Stub.getvalue res tuple field

  method getisnull tuple field =
    check_field field; check_tuple tuple;
    Stub.getisnull res tuple field

  method getlength tuple field =
    check_field field; check_tuple tuple;
    Stub.getlength res tuple field

  method cmd_status = Stub.cmd_status res
  method cmd_tuples = Stub.cmd_tuples res
  method oid_value = Stub.oid_value res

  method get_fnames = Array.init nfields (Stub.fname res)

  method get_fnames_lst =
    let lst_ref = ref [] in
    for i = nfields - 1 downto 0 do
      lst_ref := Stub.fname res i :: !lst_ref;
    done;
    !lst_ref

  method get_tuple t = check_tuple t; Array.init nfields (Stub.getvalue res t)

  method get_tuple_lst t =
    check_tuple t;
    let tpl_ref = ref [] in
    for i = nfields - 1 downto 0 do
      tpl_ref := Stub.getvalue res t i :: !tpl_ref;
    done;
    !tpl_ref

  method get_all =
    Array.init ntuples (fun t -> Array.init nfields (Stub.getvalue res t))

  method get_all_lst =
    let lst_ref = ref [] in
    let nfields_1 = nfields - 1 in
    for t = ntuples - 1 downto 0 do
      let tpl_ref = ref [] in
      for i = nfields_1 downto 0 do
        tpl_ref := Stub.getvalue res t i :: !tpl_ref
      done;
      lst_ref := !tpl_ref :: !lst_ref
    done;
    !lst_ref
end


(* Connections *)

external conndefaults : unit -> conninfo_option array = "PQconndefaults_stub"

exception Finally of exn * exn

let protectx ~f x ~(finally : 'a -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res

class connection ?host ?hostaddr ?port ?dbname ?user ?password ?options ?tty
    ?requiressl ?conninfo =

  let conn_info =
    match conninfo with
    | Some conn_info -> conn_info
    | None ->
        let b = Buffer.create 512 in
        let field name = function
          | None -> ()
          | Some x ->
              Printf.bprintf b "%s='" name;
              for i = 0 to String.length x - 1 do
                if x.[i]='\''
                then Buffer.add_string b "\\'"
                else Buffer.add_char b x.[i]
              done;
              Buffer.add_string b "' " in
        field "host" host;
        field "hostaddr" hostaddr;
        field "port" port;
        field "dbname" dbname;
        field "user" user;
        field "password" password;
        field "options" options;
        field "tty" tty;
        field "requiressl" requiressl;
        Buffer.contents b in

  fun () ->
    let my_conn = Stub.connect conn_info in
    let () =
      if Stub.connection_status my_conn <> Ok then (
        let s = Stub.error_message my_conn in
        Stub.finish my_conn;
        raise (Error (Connection_failure s)))
      else Gc.finalise Stub.finish my_conn
    in
    let conn_mtx = Mutex.create () in
    let conn_cnd = Condition.create () in
    let conn_state = ref `Free in
    let check_null () =
      if Stub.conn_isnull my_conn then
        failwith "Postgresql.check_null: connection already finished"
    in
    let wrap_mtx f =
      Mutex.lock conn_mtx;
      protectx conn_mtx
        ~f:(fun _ ->
          check_null ();  (* Check now to avoid blocking *)
          f ())
        ~finally:Mutex.unlock
    in
    let wrap_conn ?(state = `Used) f =
      wrap_mtx (fun () ->
        while !conn_state <> `Free do Condition.wait conn_cnd conn_mtx done;
        conn_state := state);
      protectx conn_state
        ~f:(fun _ ->
          check_null ();  (* Check again in case the world has changed *)
          f my_conn)
        ~finally:(fun _ ->
          Mutex.lock conn_mtx;
          conn_state := `Free;
          Condition.signal conn_cnd;
          Mutex.unlock conn_mtx)
    in
    let signal_error conn =
      raise (Error (Connection_failure (Stub.error_message conn)))
    in
    let request_cancel () =
      wrap_mtx (fun _ ->
        match !conn_state with
        | `Finishing | `Free -> ()
        | `Used ->
            match Stub.request_cancel my_conn with
            | None -> ()
            | Some err -> raise (Error (Cancel_failure err)))
    in
    let get_str_pos_len ~loc ?pos ?len str =
      let str_len = String.length str in
      match pos, len with
      | None, None -> 0, str_len
      | Some pos, _ when pos < 0 ->
          invalid_arg (sprintf "Postgresql.%s: pos < 0" loc)
      | _, Some len when len < 0 ->
          invalid_arg (sprintf "Postgresql.%s: len < 0" loc)
      | Some pos, None when pos > str_len ->
          invalid_arg (sprintf "Postgresql.%s: pos > length(str)" loc)
      | Some pos, None -> pos, str_len - pos
      | None, Some len when len > str_len ->
          invalid_arg (sprintf "Postgresql.%s: len > length(str)" loc)
      | None, Some len -> 0, len
      | Some pos, Some len when pos + len > str_len ->
          invalid_arg (sprintf "Postgresql.%s: pos + len > length(str)" loc)
      | Some pos, Some len -> pos, len
    in

object (self)
  (* Main routines *)

  method finish = wrap_conn ~state:`Finishing Stub.finish

  method try_reset =
    wrap_conn (fun conn ->
    if Stub.connection_status conn = Bad then (
      Stub.reset conn;
      if Stub.connection_status conn <> Ok then signal_error conn))

  method reset = wrap_conn Stub.reset


  (* Asynchronous Notification *)

  method notifies = wrap_conn Stub.notifies


  (* Control Functions *)

  method set_notice_processor f =
    wrap_conn (fun conn -> Stub.set_notice_processor conn f)


  (* Accessors *)

  method db = wrap_conn Stub.db
  method user = wrap_conn Stub.user
  method pass = wrap_conn Stub.pass
  method host = wrap_conn Stub.host
  method port = wrap_conn Stub.port
  method tty = wrap_conn Stub.tty
  method options = wrap_conn Stub.options
  method status = wrap_conn Stub.connection_status
  method error_message = wrap_conn Stub.error_message
  method backend_pid = wrap_conn Stub.backend_pid


  (* Commands and Queries *)

  method empty_result status =
    new result (wrap_conn (fun conn -> (Stub.make_empty_res conn status)))

  method exec ?(expect = []) ?(params = [||]) ?(binary_params = [||]) query =
    let r =
      wrap_conn (fun conn ->
        let r = Stub.exec_params conn query params binary_params in
        if Stub.result_isnull r then signal_error conn
        else r)
    in
    let res = new result r in
    let stat = res#status in
    if not (expect = []) && not (List.mem stat expect) then
      raise (Error (Unexpected_status (stat, res#error, expect)))
    else res

  method describe_prepared query =
    new result (
      wrap_conn (fun conn ->
        let r = Stub.describe_prepared conn query in
        if Stub.result_isnull r then signal_error conn
        else r))

  method send_query ?(params = [||]) ?(binary_params = [||]) query =
    wrap_conn (fun conn ->
      if Stub.send_query_params conn query params binary_params <> 1 then
        signal_error conn)

  method get_result =
    let res = wrap_conn Stub.get_result in
    if Stub.result_isnull res then None else Some (new result res)


  (* Copy operations *)

  (* Low level *)

  method getline ?(pos = 0) ?len buf =
    let buf_len = String.length buf in
    let len = match len with Some len -> len | None -> buf_len - pos in
    if len < 0 || pos < 0 || pos + len > buf_len then
      invalid_arg "Postgresql.connection#getline";
    wrap_conn (fun conn ->
      match Stub.getline conn buf pos len with
      | -1 -> EOF
      | 0 -> LineRead
      | 1 -> BufFull
      | _ -> assert false)

  method getline_async ?(pos = 0) ?len buf =
    let buf_len = String.length buf in
    let len = match len with Some len -> len | None -> buf_len - pos in
    if len < 0 || pos < 0 || pos + len > buf_len then
      invalid_arg "Postgresql.connection#getline_async";
    wrap_conn (fun conn ->
      match Stub.getline_async conn buf pos len with
      | -1 -> if Stub.endcopy conn <> 0 then signal_error conn else EndOfData
      | 0 -> NoData
      | n when n > 0 ->
         if buf.[pos + n - 1] = '\n' then DataRead n else PartDataRead n
      | _ -> assert false)

  method putline buf =
    wrap_conn (fun conn -> if Stub.putline conn buf <> 0 then signal_error conn)

  method putnbytes ?(pos = 0) ?len buf =
    let buf_len = String.length buf in
    let len = match len with Some len -> len | None -> buf_len - pos in
    if len < 0 || pos < 0 || pos + len > buf_len then
      invalid_arg "Postgresql.connection#putnbytes";
    wrap_conn (fun conn ->
      if Stub.putnbytes conn buf pos len <> 0 then signal_error conn)

  method endcopy =
    wrap_conn (fun conn -> if Stub.endcopy conn <> 0 then signal_error conn)


  (* High level *)

  method copy_out f =
    let buf = Buffer.create 1024 in
    let len = 512 in
    let s = String.create len in
    wrap_conn (fun conn ->
      let rec loop r =
        let zero = String.index s '\000' in
        Buffer.add_substring buf s 0 zero;
        match r with
         | 0 -> f (Buffer.contents buf); Buffer.clear buf; line ()
         | 1 -> loop (Stub.getline conn s 0 len)
         | _ -> f (Buffer.contents buf); self#endcopy
      and line () =
        let r = Stub.getline conn s 0 len in
        if s.[0] = '\\' && s.[1] = '.' && s.[2] = '\000' then self#endcopy
        else loop r in
      line ())

  method copy_out_channel oc =
    self#copy_out (fun s -> output_string oc (s ^ "\n"))

  method copy_in_channel ic =
    try while true do self#putline (input_line ic ^ "\n") done;
    with End_of_file -> self#putline "\\.\n"; self#endcopy


  (* Asynchronous operations and non blocking mode *)

  method set_nonblocking b =
    wrap_conn (fun conn ->
      if Stub.set_nonblocking conn b <> 0 then signal_error conn)

  method is_nonblocking = wrap_conn Stub.is_nonblocking

  method consume_input =
    wrap_conn (fun conn ->
      if Stub.consume_input conn <> 1 then signal_error conn)

  method is_busy = wrap_conn Stub.is_busy

  method flush =
    wrap_conn (fun conn -> if Stub.flush conn <> 0 then signal_error conn)

  method socket =
    wrap_conn (fun conn ->
      let s = Stub.socket conn in
      if s = -1 then signal_error conn else s)

  method request_cancel = request_cancel ()


  (* Large objects *)

  method lo_creat =
    wrap_conn (fun conn ->
      let lo = Stub.lo_creat conn in
      if lo <= 0 then signal_error conn;
      lo)

  method lo_import filename =
    wrap_conn (fun conn ->
      let oid = Stub.lo_import conn filename in
      if oid = 0 then signal_error conn;
      oid)

  method lo_export oid filename =
    wrap_conn (fun conn ->
      if Stub.lo_export conn oid filename <= 0 then signal_error conn)

  method lo_open oid =
    wrap_conn (fun conn ->
      let lo = Stub.lo_open conn oid in
      if lo = -1 then signal_error conn;
      lo)

  method lo_write ?(pos = 0) ?len buf lo =
    let buf_len = String.length buf in
    let len = match len with Some len -> len | None -> buf_len - pos in
    if len < 0 || pos < 0 || pos + len > String.length buf then
      invalid_arg "Postgresql.connection#lo_write";
    wrap_conn (fun conn ->
      let w = Stub.lo_write conn lo buf pos len in
      if w < len then signal_error conn)

  method lo_read lo ?(pos = 0) ?len buf =
    let buf_len = String.length buf in
    let len = match len with Some len -> len | None -> buf_len - pos in
    if len < 0 || pos < 0 || pos + len > String.length buf then
      invalid_arg "Postgresql.connection#lo_read";
    wrap_conn (fun conn ->
      let read = Stub.lo_read conn lo buf pos len in
      if read = -1 then signal_error conn;
      read)

  method lo_seek ?(pos = 0) ?(whence = SEEK_SET) lo =
    wrap_conn (fun conn ->
      if Stub.lo_seek conn lo pos whence < 0 then signal_error conn)

  method lo_tell lo =
    wrap_conn (fun conn ->
      let pos = Stub.lo_tell conn lo in
      if pos = -1 then signal_error conn;
      pos)

  method lo_close oid =
    wrap_conn (fun conn ->
      if Stub.lo_close conn oid = -1 then signal_error conn)

  method lo_unlink oid =
    wrap_conn (fun conn ->
      let oid = Stub.lo_unlink conn oid in
      if oid = -1 then signal_error conn)


  (* Escaping *)

  method escape_string ?pos ?len str =
    let pos, len = get_str_pos_len ~loc:"escape_string" ?pos ?len str in
    wrap_conn (fun conn -> Stub.escape_string_conn conn str ~pos ~len)

  method escape_bytea ?pos ?len str =
    let pos, len = get_str_pos_len ~loc:"escape_bytea" ?pos ?len str in
    wrap_conn (fun conn -> Stub.escape_bytea_conn conn str ~pos ~len)

end
