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

(** Client-interface to the PostgreSQL database. *)

(** Please learn about more details in the database documentation! *)

(** {6 Types} *)

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


(** {6 Exceptions and error handling} *)

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


(** {6 Utility functions}*)

val unescape_bytea : string -> string
(** [unescape_bytea str] unescapes binary string [str].  This function
    supports the new hex format for encoding bytea strings (introduced
    in Postgresql 9.0) even if the local libpq library is from an
    older version. *)

external ftype_of_oid : oid -> ftype = "ftype_of_oid_stub"
(** [ftype_of_oid oid] converts [oid] to an [ftype]. *)

external oid_of_ftype : ftype -> oid = "oid_of_ftype_stub"
(** [oid_of_ftype ftype] converts [ftype] to an [oid]. *)

val string_of_ftype : ftype -> string
(** [string_of_ftype ftype] converts [ftype] to a string. *)

val ftype_of_string : string -> ftype
(** [string_of_ftype ftype] converts [ftype] to a string. *)


(** {6 Handling results of commands and queries} *)

external result_status : result_status -> string = "PQresStatus_stub"
(** [result_status stat] convert status [stat] to a human-readable message *)

val invalid_oid : oid
(** [invalid_oid] invalid Oid. *)


(** {6 Query parameters} *)

val null : string
(** [null] can be used as an element of the optional argument [parameters]
    passed to the [exec] or [send_query] method to indicate a NULL value. *)


(** Class type of query results.

    Indices of tuples and fields start at 0!
*)
class type result = object
  (** Main routines *)

  method status : result_status
  (** [#status] @return status of a command/query result. *)

  method error : string
  (** [#error] @return error string of a result. *)


  (** Retrieving SELECT Result Information *)

  method ntuples : int
  (** [#ntuples] @return the number of tuples of a query result. *)

  method nparams : int
  (** [#nparams] @return the number of parameters of a prepared
      statement.  This function is only useful when inspecting the result
      of [#describe_prepared].  For other types of queries it will return
      zero. *)

  method nfields : int
  (** [#nfields] @return the number of fields in a query result. *)

  method fname : int -> string
  (** [#fname n] @return the name of the [n]th field.

      @raise Error if field out of range.
  *)

  method fnumber : string -> int
  (** [#fnumber field] @return the index of the field named [field].

      @raise Not_found if no such named field.
  *)

  method fformat : int -> FFormat.t
  (** [#fformat n] @return the format of the [n]th field.

      @raise Error if field out of range.
  *)

  method ftype : int -> ftype
  (** [#ftype n] @return the type of the [n]th field.

      @raise Oid if there was no corresponding ftype for the internal [oid].
      @raise Error if field out of range.
  *)

  method paramtype : int -> ftype
  (** [#paramtype n] @return the datatype of the indicated statement
      parameter.  Parameter numbers start at 0.  This function is
      only useful when inspecting the result of [#describe_prepared].
      For other types of queries it will return zero.

      @raise Oid if there was no corresponding ftype for the internal [oid].
      @raise Error if field out of range.
  *)

  method fmod : int -> int
  (** [#fmod n] @return type-specific modification data of the [n]th field.

      @raise Error if field out of range.
  *)

  method fsize : int -> int
  (** [#fsize n] @return size in bytes of the [n]th field.

      @raise Error if field out of range.
  *)

  method binary_tuples : bool
  (** [#binary_tuples] @return [true] iff result contains binary tuple data. *)


  (** Retrieving SELECT Result Values *)

  method getvalue : int -> int -> string
  (** [#getvalue tuple field] @return value of [field] in [tuple].

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)

  method getisnull : int -> int -> bool
  (** [#getisnull tuple field] tests for a NULL-value of [field] in [tuple].

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)

  method getlength : int -> int -> int
  (** [#getlength tuple field] @return length of value in [field] of
      [tuple] in bytes.

      @raise Error if tuple out of range.
      @raise Error if field out of range.
  *)


  (** Retrieving Non-SELECT Result Information *)

  method cmd_status : string
  (** [#cmd_status] @return status of SQL-command associated with result. *)

  method cmd_tuples : string
  (** [#cmd_tuples] @return number of rows affected by the SQL command. *)

  method oid_value : oid
  (** [#cmd_tuples] @return the object ID of the inserted row if the SQL
      command was an INSERT that inserted exactly one row into a table
      that has OIDs. Otherwise, returns [invalid_oid]. *)


  (** High-level routines *)

  method get_fnames : string array
  (** [#get_fnames] @return array of field names. *)

  method get_fnames_lst : string list
  (** [#get_fnames_lst] @return list of field names. *)

  method get_tuple : int -> string array
  (** [#get_tuple n] @return all fields of the [n]th tuple.

      @raise Error if tuple out of range.
  *)

  method get_tuple_lst : int -> string list
  (** [#get_tuple_lst n] @return all fields of the [n]th tuple as list.

      @raise Error if tuple out of range.
  *)

  method get_all : string array array
  (** [#get_all] @return all tuples with all fields. *)

  method get_all_lst : string list list
  (** [#get_all] @return all tuples with all fields as lists. *)
end


(** {6 Handling database connections} *)

(** Status of a connection *)
type connection_status = Ok | Bad

(** Record of connection options *)
type conninfo_option =
  {
    cio_keyword : string;  (** Keyword of option *)
    cio_envvar : string;  (** Fallback environment variable name *)
    cio_compiled : string option;  (** Fallback compiled in default value *)
    cio_val : string option;  (** Current value of option, or NULL *)
    cio_label : string;  (** Label for field in connect dialog *)
    cio_dispchar : string;  (** Character to display for this field in dialog *)
    cio_dispsize : int;  (** Field size in characters for dialog *)
  }

external conndefaults : unit -> conninfo_option array = "PQconndefaults_stub"
(** [conndefaults ()] @return array of all records of type [conninfo_option] *)


(** Class of connections.

    When [conninfo] is given, it will be used instead of all other
    optional arguments.
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
  unit ->
  (** @raise Error if there is a connection failure. *)
object
  (** Main routines *)

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

  method notifies : (string * int) option
  (** [#notifies] @return [Some (name, pid)] if available ([None]
      otherwise), where [name] is the name the of relation containing
      data, [pid] the process id of the backend.

      @raise Error if there is a connection error.
  *)


  (** Control Functions *)

  method set_notice_processor : (string -> unit) -> unit
  (** [#set_notice_processor] controls reporting of notice and warning
      messages generated by a connection.

      @raise Error if there is a connection error.
  *)


  (** Accessors *)

  method db : string
  (** [#db] @return database name of the connection.

      @raise Error if there is a connection error.
  *)

  method user : string
  (** [#user] @return user name of the connection.

      @raise Error if there is a connection error.
  *)

  method pass : string
  (** [#pass] @return password of the connection.

      @raise Error if there is a connection error.
  *)

  method host : string
  (** [#host] @return server host name of the connection.

      @raise Error if there is a connection error.
  *)

  method port : string
  (** [#port] @return port of the connection.

      @raise Error if there is a connection error.
  *)

  method tty : string
  (** [#tty] @return debug tty of the connection.

      @raise Error if there is a connection error.
  *)

  method options : string
  (** [#options] @return backend options of the connection.

      @raise Error if there is a connection error.
  *)

  method status : connection_status
  (** [#status] @return current connection status.

      @raise Error if there is a connection error.
  *)

  method error_message : string
  (** [#error_message] @return most recent error message of the connection.

      @raise Error if there is a connection error.
  *)

  method backend_pid : int
  (** [#backend] @return process id of the backend server of the connection.

      @raise Error if there is a connection error.
  *)


  (** Commands and Queries *)

  method empty_result : result_status -> result
  (** [empty_result stat] @return dummy result with a given status [stat].

      @raise Error if there is a connection error.
  *)

  method exec :
    ?expect : result_status list -> ?params : string array ->
    ?binary_params : bool array -> string -> result
  (** [exec ?expect ?params ?binary_params query] synchronous execution
      of query or command [query].  The result status will be checked
      against all elements in [expect].  If [expect] is not empty and if
      there is no match, the exception [Unexpected_status] will be raised.

      Additional query parameters can be passed in the [params] array.
      They must not be escaped and they can be referred to in [query]
      as $1, $2, ...  The value [null] can be used in the [params]
      array to denote an SQL NULL. It is possible to specify that some
      of the query parameters are passed as binary strings using the
      [binary_params] array.

      If no (or an empty) query parameter is passed, it is possible to
      emit several commands with a single call.

      @return result of query.

      @param expect default = []
      @param params default = [||]
      @param binary_params default = [||]

      @raise Error if there is a connection error.
      @raise Error if there is an unexpected result status.
  *)

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
    ?params : string array -> ?binary_params : bool array
    -> string -> unit
  (** [send_query ?params ?binary_params query] asynchronous execution
      of query or command [query].

      Additional query parameters can be passed in the [params] array.
      They must not be escaped and they can be referred to in [query]
      as $1, $2, ...   The value [null] can be used in the [params]
      array to denote an SQL NULL. It is possible to specify that some
      of the query parameters are passed as binary strings using the
      [binary_params] array.

      If no (or an empty) query parameter is passed, it is possible to
      emit several commands with a single call.

      @param params default = [||]
      @param binary_params default = [||]

      @raise Error if there is a connection error.
  *)

  method get_result : result option
  (** [get_result] @return [Some result] of an asynchronous query if
      available or [None].

      @raise Error if there is a connection error.
  *)


  (** Copy operations *)

  (** Low level *)

  method getline : ?pos : int -> ?len : int -> string -> getline_result
  (** [getline ?pos ?len buf] reads a newline-terminated line of at most
      [len] characters into [buf] starting at position [pos].

      @return getline_result

      @param pos default = 0
      @param len default = String.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if there is a connection error.
  *)

  method getline_async :
    ?pos : int -> ?len : int -> string -> getline_async_result
  (** [getline_async ?pos ?len buf] reads a newline-terminated line of
      at most [len] characters into [buf] starting at position [pos]
      (asynchronously). No need to call [endcopy] after receiving
      [EndOfData].

      @return getline_async_result

      @param pos default = 0
      @param len default = String.length buf - pos

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

  method set_nonblocking : bool -> unit
  (** [set_nonblocking b] sets state of the connection to nonblocking if
      [b] is true and to blocking otherwise.

      @raise Error if there is a connection error.
  *)

  method is_nonblocking : bool
  (** [is_nonblocking] @return the blocking status of the connection.

      @raise Error if there is a connection error.
  *)

  method consume_input : unit
  (** [consume_input] consume any available input from backend.

      @raise Error if there is a connection error.
  *)

  method is_busy : bool
  (** [is_busy] @return busy status of a query.

      @raise Error if there is a connection error.
  *)

  method flush : unit
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

  method lo_read : large_object -> ?pos : int -> ?len : int -> string -> int
  (** [lo_read lo ?pos ?len buf] reads [len] bytes from large object [lo]
      to buffer [buf] starting at position [pos].

      @param pos default = 0
      @param len default = String.length buf - pos

      @raise Invalid_argument if the buffer parameters are invalid.
      @raise Error if [len] bytes could not be read.
      @raise Error if there is a connection error.
  *)

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
  (** [lo_tell lo] @return current read/write position of large object [lo].

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
