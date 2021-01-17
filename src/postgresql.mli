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
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Client-interface to the PostgreSQL database. *)

(** Please learn about more details in the database documentation! *)

(** {2 Types} *)

(** Object ID (= Postgresql type of an object) *)
type oid = int

(** Handle for large objects *)
type large_object

(** Type of field formats *)
module FFormat : sig
  type t =
    | TEXT
    | BINARY
end

(** Type of fields *)
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
  | JSON
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
  | JSONB

module Error_field = Error_field
module Error_code = Error_code

(** Status of command/query results *)
type result_status =
  | Empty_query     (** String sent to the backend was empty *)
  | Command_ok      (** Successful completion of a command returning no data *)
  | Tuples_ok       (** The query successfully executed *)
  | Copy_out        (** Copy Out (from server) data transfer started *)
  | Copy_in         (** Copy In (to server) data transfer started *)
  | Bad_response    (** The server's response was not understood *)
  | Nonfatal_error
  | Fatal_error
  | Copy_both
  | Single_tuple    (** One tuple of a result set ({!set_single_row_mode}) *)

(** Result of put_copy_data and put_copy_end *)
type put_copy_result =
  | Put_copy_queued     (** Data queued *)
  | Put_copy_not_queued (** Data not queued due to full bufffers (async only) *)
  | Put_copy_error      (** Copying failed, see [#error_message] for details *)

(** Result of get_copy_data *)
type get_copy_result =
  | Get_copy_data of string (** Data corresponding to one row is returned *)
  | Get_copy_wait   (** The next row is still being received (async only); wait
                        for read-only, call [consume_input], and try again *)
  | Get_copy_end    (** All data has been successfully retrieved *)
  | Get_copy_error  (** Copying failed, see [#error_message] for details *)

(** Result of getline *)
type getline_result =
  | EOF       (** End of input reached *)
  | LineRead  (** Entire line has been read *)
  | BufFull   (** Buffer full but terminating newline not encountered *)

(** Result of getline_async *)
type getline_async_result =
  | EndOfData            (** End-of-copy-data marker recognized *)
  | NoData               (** No data available *)
  | DataRead of int      (** [DataRead n] indicates [n] bytes of read data *)
  | PartDataRead of int  (** Like [DataRead], but data only partially read *)

(** Seek command ("whence") *)
type seek_cmd =
  | SEEK_SET  (** Seek from start of large object *)
  | SEEK_CUR  (** Seek from current read/write position of large object *)
  | SEEK_END  (** Seek from end of large object *)


(** {2 Exceptions and error handling} *)

(** Kinds of exceptions:

    [Field_out_of_range (i, n)] - access to field [i] not within range [n]
    [Tuple_out_of_range (i, n)] - access to tuple [i] not within range [n]
    [Binary] - result consists of binary tuple data
    [Connection_failure msg] - connection failed due to reason [msg]
    [Unexpected_status (stat, msg, expected)] - result status [stat] was not in
                                                [expected] due to error [msg]
*)
type error =
  | Field_out_of_range of int * int
  | Tuple_out_of_range of int * int
  | Binary
  | Connection_failure of string
  | Unexpected_status of result_status * string * (result_status list)
  | Cancel_failure of string

(** [Error error] indicates an [error] *)
exception Error of error

val string_of_error : error -> string
(** [string_of_error error] convert [error] to a human-readable message *)

(** [Oid oid] conversion from an oid to an ftype encountered an unknown [oid] *)
exception Oid of oid


(** {2 Utility functions}*)

val unescape_bytea : string -> string
(** [unescape_bytea str] unescapes binary string [str].  This function
    supports the new hex format for encoding bytea strings (introduced
    in Postgresql 9.0) even if the local libpq library is from an
    older version. *)

val ftype_of_oid : oid -> ftype
(** [ftype_of_oid oid] converts [oid] to an [ftype]. *)

val oid_of_ftype : ftype -> oid
(** [oid_of_ftype ftype] converts [ftype] to an [oid]. *)

val string_of_ftype : ftype -> string
(** [string_of_ftype ftype] converts [ftype] to a string. *)

val ftype_of_string : string -> ftype
(** [string_of_ftype ftype] converts string to a [ftype]. *)


(** {2 Handling results of commands and queries} *)

val result_status : result_status -> string
(** [result_status stat] convert status [stat] to a human-readable message *)

val invalid_oid : oid
(** [invalid_oid] invalid Oid. *)


(** {2 Query parameters} *)

val null : string
(** [null] can be used as an element of the optional argument [parameters]
    passed to the [exec] or [send_query] method to indicate a NULL value. *)


(** Class type of query results.

    Indices of tuples and fields start at 0!
*)
class type result = object
  (* Main routines *)

  method status : result_status
  (** [#status]

      @return status of a command/query result.
  *)

  method error : string
  (** [#error]

      @return error string of a result.
  *)

  method error_field : Error_field.t -> string
  (** [#error_field]

      @return message of given error field in a result.
  *)

  method error_code : Error_code.t
  (** [#error_code]

      @return the error code of the error condition as stored in the
      SQLSTATE field.
  *)

  (** Retrieving SELECT Result Information *)

  method ntuples : int
  (** [#ntuples]

      @return the number of tuples of a query result.
  *)

  method nparams : int
  (** [#nparams]

      @return the number of parameters of a prepared statement.  This function
      is only useful when inspecting the result of [#describe_prepared].
      For other types of queries it will return zero.
  *)

  method nfields : int
  (** [#nfields]

      @return the number of fields in a query result.
  *)

  method fname : int -> string
  (** [#fname n]

      @return the name of the [n]th field.

      @raise Error if field out of range.
  *)

  method fnumber : string -> int
  (** [#fnumber field]

      @return the index of the field named [field].

      @raise Not_found if no such named field.
  *)

  method fformat : int -> FFormat.t
  (** [#fformat n]

      @return the format of the [n]th field.

      @raise Error if field out of range.
  *)

  method ftype : int -> ftype
  (** [#ftype n]

      @return the type of the [n]th field.

      @raise Oid if there was no corresponding ftype for the internal [oid].
      @raise Error if field out of range.
  *)

  method ftype_oid : int -> oid
  (** [#ftype n]

      @return the oid of the [n]th field.

      @raise Error if field out of range.
  *)

  method paramtype : int -> ftype
  (** [#paramtype n]

      @return the datatype of the indicated statement parameter.  Parameter
      numbers start at 0.  This function is only useful when inspecting the
      result of [#describe_prepared].  For other types of queries it will
      return zero.

      @raise Oid if there was no corresponding ftype for the internal [oid].
      @raise Error if field out of range.
  *)

  method paramtype_oid : int -> oid
  (** [#paramtype n]

      @return the oid of the indicated statement parameter.  Parameter numbers
      start at 0.  This function is only useful when inspecting the result of
      [#describe_prepared].  For other types of queries it will return zero.

      @raise Error if field out of range.
  *)

  method fmod : int -> int
  (** [#fmod n]

      @return type-specific modification data of the [n]th field.

      @raise Error if field out of range.
  *)

  method fsize : int -> int
  (** [#fsize n]

      @return size in bytes of the [n]th field.

      @raise Error if field out of range.
  *)

  method binary_tuples : bool
  (** [#binary_tuples]

      @return [true] iff result contains binary tuple data.
  *)


  (** Retrieving SELECT Result Values *)

  method getvalue : int -> int -> string
  (** [#getvalue tuple field]

      @return value of [field] in [tuple].

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)

  method get_escaped_value : int -> int -> string
  (** [#get_escaped_value tuple field]

      @return escaped value of [field] in [tuple].

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)

  method getisnull : int -> int -> bool
  (** [#getisnull tuple field] tests for a NULL-value of [field] in [tuple].

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)

  method getlength : int -> int -> int
  (** [#getlength tuple field]

      @return length of value in [field] of [tuple] in bytes.

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)


  (** Retrieving Non-SELECT Result Information *)

  method cmd_status : string
  (** [#cmd_status]

      @return status of SQL-command associated with result.
  *)

  method cmd_tuples : string
  (** [#cmd_tuples]

      @return number of rows affected by the SQL command.
  *)

  method oid_value : oid
  (** [#cmd_tuples]

      @return the object ID of the inserted row if the SQL command was
      an INSERT that inserted exactly one row into a table that has
      OIDs. Otherwise, returns [invalid_oid].
  *)


  (** High-level routines *)

  method get_fnames : string array
  (** [#get_fnames]

      @return array of field names.
  *)

  method get_fnames_lst : string list
  (** [#get_fnames_lst]

      @return list of field names.
  *)

  method get_tuple : int -> string array
  (** [#get_tuple n]

      @return all fields of the [n]th tuple.

      @raise Error if tuple out of range.
  *)

  method get_tuple_lst : int -> string list
  (** [#get_tuple_lst n]

      @return all fields of the [n]th tuple as list.

      @raise Error if tuple out of range.
  *)

  method get_all : string array array
  (** [#get_all]

      @return all tuples with all fields.
  *)

  method get_all_lst : string list list
  (** [#get_all]

      @return all tuples with all fields as lists.
  *)
end


(** {2 Handling database connections} *)

(** Status of a connection *)
type connection_status =
  | Ok | Bad
  (* Non-blocking: *)
  | Connection_started
  | Connection_made
  | Connection_awaiting_response
  | Connection_auth_ok
  | Connection_setenv
  | Connection_ssl_startup

(** Polling status used while establishing a connection asynchronously. *)
type polling_status =
  | Polling_failed
  | Polling_reading
  | Polling_writing
  | Polling_ok

(** Result of a call to flush on nonblocking connections. *)
type flush_status =
  | Successful
  | Data_left_to_send

(** Record of connection options *)
type conninfo_option =
  {
    cio_keyword : string;  (** Keyword of option *)
    cio_envvar : string option;  (** Fallback environment variable name *)
    cio_compiled : string option;  (** Fallback compiled in default value *)
    cio_val : string option;  (** Current value of option, or NULL *)
    cio_label : string;  (** Label for field in connect dialog *)
    cio_dispchar : string;  (** Character to display for this field in dialog *)
    cio_dispsize : int;  (** Field size in characters for dialog *)
  }

(** Type of asynchronous notifications *)
module Notification : sig
  type t = {
    name : string;  (** name the of relation containing data *)
    pid : int;  (** the process id of the backend *)
    extra : string;  (** payload data (empty if not provided) *)
  }
end  (* Notification *)

val conndefaults : unit -> conninfo_option array
(** [conndefaults ()]

    @return array of all records of type [conninfo_option]
*)


(** Class of connections.

    When [conninfo] is given, it will be used instead of all other
    optional arguments.

    @param startonly If true, initiate a non-blocking connect procedure, which
      involves cooperative calls to {!connect_poll} before the connection is
      usable.

    @raise Error if there is a connection failure.
*)
class connection :
  ?host : string ->  (* Default: none *)
  ?hostaddr : string ->  (* Default: none *)
  ?port : string  ->  (* Default: none *)
  ?dbname : string ->  (* Default: none *)
  ?user : string ->  (* Default: none *)
  ?password : string ->  (* Default: none *)
  ?options : string ->  (* Default: none *)
  ?tty : string ->  (* Default: none *)
  ?requiressl : string ->  (* Default: none *)
  ?conninfo : string ->  (* Default: none *)
  ?startonly : bool -> (* Default: false *)
  unit ->
object
  (* Main routines *)

  method finish : unit
  (** [#finish] closes the connection.

      @raise Error if there is a connection error.
  *)

  method try_reset : unit
  (** [#try_reset] tries to reset the connection if it is bad. If
      resetting fails, the [error] exception will be raised with
      [Connection_failure].

      @raise Error if there is a connection error.
  *)

  method reset : unit
  (** [#reset] resets the connection.

      @raise Error if there is a connection error.
  *)


  (** Asynchronous Notification *)

  method notifies : Notification.t option
  (** [#notifies]

      @return [Some notification] if available ([None] otherwise).

      @raise Error if there is a connection error.
  *)


  (** Control Functions *)

  method set_notice_processor : (string -> unit) -> unit
  (** [#set_notice_processor] controls reporting of notice and warning
      messages generated by a connection.

      {e Warning:} This function is unsafe in combination with a number of libpq
      entry points, and should not be used for now. As a workaround,
      {!#set_notice_processing} can be used to silence notices, if this is more
      appropriate than the default behaviour of printing them to standard error.

      @raise Error if there is a connection error.
  *)

  method set_notice_processing : [`Stderr | `Quiet] -> unit
  (** [#set_notice_processing] controls reporting of notice and warning messages
      generated by a connection by providing predefined callbacks.

      @raise Error if there is a connection error.
  *)


  (** Accessors *)

  method db : string
  (** [#db]

      @return database name of the connection.

      @raise Error if there is a connection error.
  *)

  method user : string
  (** [#user]

      @return user name of the connection.

      @raise Error if there is a connection error.
  *)

  method pass : string
  (** [#pass]

      @return password of the connection.

      @raise Error if there is a connection error.
  *)

  method host : string
  (** [#host]

      @return server host name of the connection.

      @raise Error if there is a connection error.
  *)

  method port : string
  (** [#port]

      @return port of the connection.

      @raise Error if there is a connection error.
  *)

  method tty : string
  (** [#tty]

      @return debug tty of the connection.

      @raise Error if there is a connection error.
  *)

  method options : string
  (** [#options]

      @return backend options of the connection.

      @raise Error if there is a connection error.
  *)

  method status : connection_status
  (** [#status]

      @return current connection status.

      @raise Error if there is a connection error.
  *)

  method error_message : string
  (** [#error_message]

      @return most recent error message of the connection.

      @raise Error if there is a connection error.
  *)

  method backend_pid : int
  (** [#backend]

      @return process id of the backend server of the connection.

      @raise Error if there is a connection error.
  *)

  method server_version : int * int * int
  (* [#server_version]

      @return (major, minor, revision).

      @raise Error if there is a connection error.
  *)


  (** Commands and Queries *)

  method empty_result : result_status -> result
  (** [empty_result stat]

      @return dummy result with a given status [stat].

      @raise Error if there is a connection error.
  *)

  method exec :
    ?expect : result_status list ->
    ?param_types : oid array -> ?params : string array ->
    ?binary_params : bool array -> ?binary_result : bool ->
    string -> result
  (** [exec ?expect ?params ?param_types ?binary_params ?binary_result query]
      synchronous execution of query or command [query].  The result
      status will be checked against all elements in [expect].  If
      [expect] is not empty and if there is no match, the exception
      [Unexpected_status] will be raised.

      Additional query parameters can be passed in the [params] array.
      They must not be escaped and they can be referred to in [query]
      as $1, $2, ...  The value [null] can be used in the [params]
      array to denote an SQL NULL. It is possible to specify that some
      of the query parameters are passed as binary strings using the
      [binary_params] array.  By default, results are returned in text
      format, but will be returned in binary format if [binary_result]
      is true.

      If no (or an empty) query parameter is passed, it is possible to
      emit several commands with a single call.

      @return result of query.

      @param expect default = []
      @param param_types default = [||]
      @param params default = [||]
      @param binary_params default = [||]
      @param binary_result default = false

      @raise Error if there is a connection error.
      @raise Error if there is an unexpected result status.
  *)

  method prepare : ?param_types : oid array -> string -> string -> result
  (** [prepare ?param_types stm_name query] creates a prepared query named
      [stm_name] which will execute the query or command [query] when passed to
      [#exec_prepared]. *)

  method exec_prepared :
    ?expect : result_status list -> ?params : string array ->
    ?binary_params : bool array -> string -> result
  (** [exec_prepared ?expect ?params ?binary_params stm_name] acts as
      [#exec], except executes the prepared query [stm_name]. *)

  method describe_prepared : string -> result
  (** [#describe_prepared stm_name] submits a request to obtain
      information about the specified prepared statement, and waits for
      completion.  {!describe_prepared} allows an application to obtain
      information about a previously prepared statement.  The [stm_name]
      parameter can be the empty string ("") to reference the unnamed
      statement, otherwise it must be the name of an existing prepared
      statement.  On success, a {!result} with status [Command_ok] is
      returned.  The methods {!result.nparams} and {!result.paramtype}
      of the class [result] can be used to obtain information about
      the parameters of the prepared statement, and the methods
      {!result.nfields}, {!result.fname} and {!result.ftype} provide
      information about the result columns (if any) of the statement.

      To prepare a statement use the SQL command PREPARE.

      @param stm_name The name of the previously prepared query

      @raise Error if there is a connection error.

      @see <http://www.postgresql.org/docs/8.3/interactive/sql-prepare.html>
      PostgreSQL documentation about [PREPARE]
  *)

  method send_query :
    ?param_types : oid array -> ?params : string array ->
    ?binary_params : bool array -> string -> unit
  (** [send_query ?param_types ?params ?binary_params query] asynchronous
      execution of query or command [query].

      Additional query parameters can be passed in the [params] array.
      They must not be escaped and they can be referred to in [query]
      as $1, $2, ...   The value [null] can be used in the [params]
      array to denote an SQL NULL. It is possible to specify that some
      of the query parameters are passed as binary strings using the
      [binary_params] array.

      If no (or an empty) query parameter is passed, it is possible to
      emit several commands with a single call.

      @param param_types default = [||]
      @param params default = [||]
      @param binary_params default = [||]

      @raise Error if there is a connection error.
  *)

  method send_prepare : ?param_types : oid array -> string -> string -> unit
  (** [#send_prepare ?param_types stm_name query] sends a query preparation
      without waiting for the result.  This does the same as {!prepare} except
      that the status is reported by {!get_result} when available.

      @raise Error if there is a connection error. *)

  method send_query_prepared :
    ?params : string array -> ?binary_params : bool array
    -> string -> unit
  (** [#send_query_prepared ?params ?binary_params stm_name] is an
      asynchronous version of {!query_prepared}.  The semantics is otherwise
      the same, and the result is reported by {!get_result} when available.

      @param params default = [||]
      @param binary_params default = [||]

      @raise Error if there is a connection error. *)

  method send_describe_prepared : string -> unit
  (** [#send_describe_prepared stm_name] sends a request for a description of
      a prepared query without waiting for the result.  The result must be
      fetched with {!get_result} when it becomes available.  Otherwise it
      does the same as {!describe_prepared}.

      @raise Error if there is a connection error. *)

  method send_describe_portal : string -> unit
  (** [#send_describe_portal portal_name] sends a request for a description of
      the named portal.  The result must be fetched with {!get_result}.

      @raise Error if there is a connection error. *)

  method set_single_row_mode : unit
  (** [#set_single_row_mode] called right after {!send_query} or a sibling
      function causes the returned rows to be split into individual results. *)

  method get_result : result option
  (** [get_result]

      @return [Some result] of an asynchronous query if available or [None].

      @raise Error if there is a connection error.
  *)


  (** Copy operations *)

  (** Low level *)

  method put_copy_data : ?pos : int -> ?len : int -> string -> put_copy_result
  (** [put_copy_data ?pos ?len buf] sends [buf] of length [len] starting at
      [pos] to the backend server, which must be in copy-in mode.  In
      non-blocking mode, returns {!Put_copy_not_queued} if the data was not
      queued due to full buffers.

      @param pos default = 0
      @param len default = String.length - pos

      @raise Invalid_argument if the buffer parameters are invalid.
   *)

  method put_copy_end : ?error_msg : string -> unit -> put_copy_result
  (** [put_copy_end ?error_msg ()] terminates the copy-in mode, leaving the
      connection in [Command_ok] or failed state.  In non-blocking mode, returns
      {!Put_copy_not_queued} if the termination message was not queued due to
      full buffers.  Also, to ensure delivery of data in non-blocking mode,
      repeatedly wait for write-ready an call {!#flush}.

      @param error_msg if set, force the copy operation to fail with the given
        message.
   *)

  method get_copy_data : ?async : bool -> unit -> get_copy_result
  (** [get_copy_data ?async ()] retrieves the next row of data if available.
      Only single complete rows are returned.  In synchronous mode, the call
      will wait for completion of the next row.  In asynchronous mode it will
      return immediately with [Get_copy_wait] if the row transfer is incomplete.
      In that case, wait for read-ready and call {!#consume_input} before
      retrying.

      @param async default = false
   *)

  method getline : ?pos : int -> ?len : int -> Bytes.t -> getline_result
  (** [getline ?pos ?len buf] reads a newline-terminated line of at most
      [len] characters into [buf] starting at position [pos].

      @return getline_result

      @param pos default = 0
      @param len default = Bytes.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if there is a connection error.
  *)

  method getline_async :
    ?pos : int -> ?len : int -> Bytes.t -> getline_async_result
  (** [getline_async ?pos ?len buf] reads a newline-terminated line of
      at most [len] characters into [buf] starting at position [pos]
      (asynchronously).  No need to call [endcopy] after receiving
      [EndOfData].

      @return getline_async_result

      @param pos default = 0
      @param len default = Bytes.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if there is a connection error.
  *)

  method putline : string -> unit
  (** [putline str] sends [str] to backend server. Don't use this method
      for binary data, use putnbytes instead!

      @raise Error if there is a connection error.
  *)

  method putnbytes : ?pos : int -> ?len : int -> string -> unit
  (** [putnbytes ?pos ?len buf] sends the substring of [buf] of length
      [len] starting at [pos] to backend server (use this method for
      binary data).

      @param pos default = 0
      @param len default = String.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if there is a connection error.
  *)

  method endcopy : unit
  (** [endcopy] synchronizes with the backend.

      @raise Error if there is a connection error.
  *)


  (** High level *)

  method copy_out : (string -> unit) -> unit
  (** [copy_out f] applies [f] to each line returned by backend server.

      @raise Error if there is a connection error.
  *)

  method copy_out_channel : out_channel -> unit
  (** [copy_out_channel ch] sends each line returned by backend server
      to output channel [ch].

      @raise Error if there is a connection error.
  *)

  method copy_in_channel : in_channel -> unit
  (** [copy_in_channel ch] sends each line in input channel [ch] to
      backend server.

      @raise Error if there is a connection error.
  *)


  (** Asynchronous operations and non blocking mode *)

  method connect_poll : polling_status
  (** After creating a connection with [~startonly:true], {!connect_poll}
      must be called a number of times before the connection can be used.
      The precise procedure is described in the libpq manual, but the following
      code should capture the idea, assuming monadic concurrency primitives
      [return] and [>>=] along with polling functions [wait_for_read] and
      [wait_for_write]:
      {[
        let my_async_connect () =
          let c = new connection () in
          let rec establish_connection = function
            | Polling_failed | Polling_ok -> return c
            | Polling_reading -> wait_for_read c#socket >>= fun () ->
                                 establish_connection c#connect_poll
            | Polling_writing -> wait_for_write c#socket >>= fun () ->
                                 establish_connection c#connect_poll in
          establish_connection Polling_writing
      ]}
      See also [examples/async.ml]. *)

  method reset_start : bool
  (** An asynchronous variant of {!reset}.  Use {!reset_poll} to
      finish re-establishing the connection. *)

  method reset_poll : polling_status
  (** Used analogously to {!connect_poll} after calling {!reset_start}. *)

  method set_nonblocking : bool -> unit
  (** [set_nonblocking b] sets state of the connection to nonblocking if
      [b] is true and to blocking otherwise.

      @raise Error if there is a connection error.
  *)

  method is_nonblocking : bool
  (** [is_nonblocking]

      @return the blocking status of the connection.

      @raise Error if there is a connection error.
  *)

  method consume_input : unit
  (** [consume_input] consume any available input from backend.

      @raise Error if there is a connection error.
  *)

  method is_busy : bool
  (** [is_busy]

      @return busy status of a query.

      @raise Error if there is a connection error.
  *)

  method flush : flush_status
  (** [flush] attempts to flush any data queued to the backend.

      @raise Error if there is a connection error.
  *)

  method socket : int
  (** [socket] obtains the file descriptor for the backend connection
      socket as an integer.

      @raise Error if there is a connection error.
  *)

  method request_cancel : unit
  (** [request_cancel] requests that PostgreSQL abandon processing of
      the current command.

      @raise Error if there is a connection or cancel error.
  *)


  (** Large objects *)

  method lo_creat : oid
  (** [lo_creat] creates a new large object and returns its oid.

      @raise Error if there is a connection error.
  *)

  method lo_import : string -> oid
  (** [lo_import filename] imports an operating system file given by
      [filename] as a large object.

      @raise Error if there is a connection error.
  *)

  method lo_export : oid -> string -> unit
  (** [lo_export oid filename] exports the large object given by [oid]
      to an operating system file given by [filename].
      @raise Error if there is a connection error. *)

  method lo_open : oid -> large_object
  (** [lo_open oid] opens the large object given by [oid] for reading and
      writing.

      @raise Error if there is a connection error.
  *)

  method lo_write : ?pos : int -> ?len : int -> string -> large_object -> unit
  (** [lo_write ?pos ?len buf lo] writes [len] bytes of buffer [buf]
      starting at position [pos] to large object [lo].

      @param pos default = 0
      @param len default = String.length buf - pos
      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if [len] bytes could not be written.
      @raise Error if there is a connection error. *)

  method lo_write_ba :
    ?pos : int -> ?len : int ->
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    large_object -> unit
  (** As [lo_write], but performs a zero-copy write from the given Bigarray. *)

  method lo_read : large_object -> ?pos : int -> ?len : int -> Bytes.t -> int
  (** [lo_read lo ?pos ?len buf] reads [len] bytes from large object [lo]
      to buffer [buf] starting at position [pos].

      @param pos default = 0
      @param len default = Bytes.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if [len] bytes could not be read.
      @raise Error if there is a connection error.
  *)

  method lo_read_ba :
    large_object -> ?pos : int -> ?len : int ->
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int
  (** As [lo_read], but performs a zero-copy read into the given Bigarray. *)

  method lo_seek : ?pos : int -> ?whence : seek_cmd -> large_object -> unit
  (** [lo_seek ?pos ?whence lo] seeks read/write position [pos] in
      large object [lo] relative to the start, current read/write
      position, or end of the object ([whence] is SEEK_SET, SEEK_CUR,
      SEEK_END respectively).

      @param pos default = 0
      @param whence default = [SEEK_SET]

      @raise Error if there is a connection error.
  *)

  method lo_tell : large_object -> int
  (** [lo_tell lo]

      @return current read/write position of large object [lo].

      @raise Error if there is a connection error.
  *)

  method lo_close : large_object -> unit
  (** [lo_close lo] closes large object [lo].

      @raise Error if there is a connection error.
  *)

  method lo_unlink : oid -> unit
  (** [lo_unlink oid] removes the large object specified by [oid] from the
      database.

      @raise Error if there is a connection error.
  *)


  (** Escaping *)

  method escape_string : ?pos : int -> ?len : int -> string -> string
  (** [escape_string ?pos ?len str] escapes ASCII-substring [str]
      of length [len] starting at position [pos] for use within SQL.

      @param pos default = 0
      @param len default = String.length str - pos
  *)

  method escape_bytea : ?pos : int -> ?len : int -> string -> string
  (** [escape_bytea ?pos ?len str] escapes binary substring [str]
      of length [len] starting at position [pos] for use within SQL.

      @param pos default = 0
      @param len default = String.length str - pos
  *)
end
