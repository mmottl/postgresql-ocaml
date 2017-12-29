(*
   PostgreSQL-OCAML - OCaml-interface to the PostgreSQL database

   Copyright (C) 2017  Sean Grove
   email: sean@bushi.do
   WWW:   http://www.riseos.com

   Copyright (C) 2017-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

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

type t =
  | SEVERITY
  | SEVERITY_NONLOCALIZED
  | SQLSTATE
  | MESSAGE_PRIMARY
  | MESSAGE_DETAIL
  | MESSAGE_HINT
  | STATEMENT_POSITION
  | INTERNAL_POSITION
  | INTERNAL_QUERY
  | CONTEXT
  | SCHEMA_NAME
  | TABLE_NAME
  | COLUMN_NAME
  | DATATYPE_NAME
  | CONSTRAINT_NAME
  | SOURCE_FILE
  | SOURCE_LINE
  | SOURCE_FUNCTION

let to_string = function
  | SEVERITY -> "SEVERITY"
  | SEVERITY_NONLOCALIZED -> "SEVERITY_NONLOCALIZED"
  | SQLSTATE -> "SQLSTATE"
  | MESSAGE_PRIMARY -> "MESSAGE_PRIMARY"
  | MESSAGE_DETAIL -> "MESSAGE_DETAIL"
  | MESSAGE_HINT -> "MESSAGE_HINT"
  | STATEMENT_POSITION -> "STATEMENT_POSITION"
  | INTERNAL_POSITION -> "INTERNAL_POSITION"
  | INTERNAL_QUERY -> "INTERNAL_QUERY"
  | CONTEXT -> "CONTEXT"
  | SCHEMA_NAME -> "SCHEMA_NAME"
  | TABLE_NAME -> "TABLE_NAME"
  | COLUMN_NAME -> "COLUMN_NAME"
  | DATATYPE_NAME -> "DATATYPE_NAME"
  | CONSTRAINT_NAME -> "CONSTRAINT_NAME"
  | SOURCE_FILE -> "SOURCE_FILE"
  | SOURCE_LINE -> "SOURCE_LINE"
  | SOURCE_FUNCTION -> "SOURCE_FUNCTION"
