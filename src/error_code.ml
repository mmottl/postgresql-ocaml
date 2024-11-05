(* PostgreSQL-OCAML - OCaml-interface to the PostgreSQL database

   Copyright (C) 2017 Sean Grove email: sean@bushi.do WWW: http://www.riseos.com

   Copyright (C) 2017- Markus Mottl email: markus.mottl@gmail.com WWW:
   http://www.ocaml.info

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

type t =
  | ACTIVE_SQL_TRANSACTION
  | ADMIN_SHUTDOWN
  | AMBIGUOUS_ALIAS
  | AMBIGUOUS_COLUMN
  | AMBIGUOUS_FUNCTION
  | AMBIGUOUS_PARAMETER
  | ARRAY_SUBSCRIPT_ERROR
  | ASSERT_FAILURE
  | BAD_COPY_FILE_FORMAT
  | BRANCH_TRANSACTION_ALREADY_ACTIVE
  | CANNOT_COERCE
  | CANNOT_CONNECT_NOW
  | CANT_CHANGE_RUNTIME_PARAM
  | CARDINALITY_VIOLATION
  | CASE_NOT_FOUND
  | CHARACTER_NOT_IN_REPERTOIRE
  | CHECK_VIOLATION
  | COLLATION_MISMATCH
  | CONFIGURATION_LIMIT_EXCEEDED
  | CONFIG_FILE_ERROR
  | CONNECTION_DOES_NOT_EXIST
  | CONNECTION_EXCEPTION
  | CONNECTION_FAILURE
  | CONTAINING_SQL_NOT_PERMITTED
  | CRASH_SHUTDOWN
  | DATABASE_DROPPED
  | DATATYPE_MISMATCH
  | DATA_CORRUPTED
  | DATA_EXCEPTION
  | DATETIME_FIELD_OVERFLOW
  | DEADLOCK_DETECTED
  | DEPENDENT_OBJECTS_STILL_EXIST
  | DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST
  | DEPRECATED_FEATURE
  | DIAGNOSTICS_EXCEPTION
  | DISK_FULL
  | DIVISION_BY_ZERO
  | DUPLICATE_ALIAS
  | DUPLICATE_COLUMN
  | DUPLICATE_CURSOR
  | DUPLICATE_DATABASE
  | DUPLICATE_FILE
  | DUPLICATE_FUNCTION
  | DUPLICATE_OBJECT
  | DUPLICATE_PREPARED_STATEMENT
  | DUPLICATE_SCHEMA
  | DUPLICATE_TABLE
  | DYNAMIC_RESULT_SETS_RETURNED
  | ERROR_IN_ASSIGNMENT
  | ESCAPE_CHARACTER_CONFLICT
  | EVENT_TRIGGER_PROTOCOL_VIOLATED
  | EXCLUSION_VIOLATION
  | EXTERNAL_ROUTINE_EXCEPTION
  | EXTERNAL_ROUTINE_INVOCATION_EXCEPTION
  | FDW_COLUMN_NAME_NOT_FOUND
  | FDW_DYNAMIC_PARAMETER_VALUE_NEEDED
  | FDW_ERROR
  | FDW_FUNCTION_SEQUENCE_ERROR
  | FDW_INCONSISTENT_DESCRIPTOR_INFORMATION
  | FDW_INVALID_ATTRIBUTE_VALUE
  | FDW_INVALID_COLUMN_NAME
  | FDW_INVALID_COLUMN_NUMBER
  | FDW_INVALID_DATA_TYPE
  | FDW_INVALID_DATA_TYPE_DESCRIPTORS
  | FDW_INVALID_DESCRIPTOR_FIELD_IDENTIFIER
  | FDW_INVALID_HANDLE
  | FDW_INVALID_OPTION_INDEX
  | FDW_INVALID_OPTION_NAME
  | FDW_INVALID_STRING_FORMAT
  | FDW_INVALID_STRING_LENGTH_OR_BUFFER_LENGTH
  | FDW_INVALID_USE_OF_NULL_POINTER
  | FDW_NO_SCHEMAS
  | FDW_OPTION_NAME_NOT_FOUND
  | FDW_OUT_OF_MEMORY
  | FDW_REPLY_HANDLE
  | FDW_SCHEMA_NOT_FOUND
  | FDW_TABLE_NOT_FOUND
  | FDW_TOO_MANY_HANDLES
  | FDW_UNABLE_TO_CREATE_EXECUTION
  | FDW_UNABLE_TO_CREATE_REPLY
  | FDW_UNABLE_TO_ESTABLISH_CONNECTION
  | FEATURE_NOT_SUPPORTED
  | FLOATING_POINT_EXCEPTION
  | FOREIGN_KEY_VIOLATION
  | FUNCTION_EXECUTED_NO_RETURN_STATEMENT
  | GENERATED_ALWAYS
  | GROUPING_ERROR
  | HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL
  | IDLE_IN_TRANSACTION_SESSION_TIMEOUT
  | IMPLICIT_ZERO_BIT_PADDING
  | INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION
  | INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION
  | INDETERMINATE_COLLATION
  | INDETERMINATE_DATATYPE
  | INDEX_CORRUPTED
  | INDICATOR_OVERFLOW
  | INSUFFICIENT_PRIVILEGE
  | INSUFFICIENT_RESOURCES
  | INTEGRITY_CONSTRAINT_VIOLATION
  | INTERNAL_ERROR
  | INTERVAL_FIELD_OVERFLOW
  | INVALID_ARGUMENT_FOR_LOGARITHM
  | INVALID_ARGUMENT_FOR_NTH_VALUE_FUNCTION
  | INVALID_ARGUMENT_FOR_NTILE_FUNCTION
  | INVALID_ARGUMENT_FOR_POWER_FUNCTION
  | INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION
  | INVALID_AUTHORIZATION_SPECIFICATION
  | INVALID_BINARY_REPRESENTATION
  | INVALID_CATALOG_NAME
  | INVALID_CHARACTER_VALUE_FOR_CAST
  | INVALID_COLUMN_DEFINITION
  | INVALID_COLUMN_REFERENCE
  | INVALID_CURSOR_DEFINITION
  | INVALID_CURSOR_NAME
  | INVALID_CURSOR_STATE
  | INVALID_DATABASE_DEFINITION
  | INVALID_DATETIME_FORMAT
  | INVALID_ESCAPE_CHARACTER
  | INVALID_ESCAPE_OCTET
  | INVALID_ESCAPE_SEQUENCE
  | INVALID_FOREIGN_KEY
  | INVALID_FUNCTION_DEFINITION
  | INVALID_GRANTOR
  | INVALID_GRANT_OPERATION
  | INVALID_INDICATOR_PARAMETER_VALUE
  | INVALID_LOCATOR_SPECIFICATION
  | INVALID_NAME
  | INVALID_OBJECT_DEFINITION
  | INVALID_PARAMETER_VALUE
  | INVALID_PASSWORD
  | INVALID_PREPARED_STATEMENT_DEFINITION
  | INVALID_RECURSION
  | INVALID_REGULAR_EXPRESSION
  | INVALID_ROLE_SPECIFICATION
  | INVALID_ROW_COUNT_IN_LIMIT_CLAUSE
  | INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE
  | INVALID_SAVEPOINT_SPECIFICATION
  | INVALID_SCHEMA_DEFINITION
  | INVALID_SCHEMA_NAME
  | INVALID_SQLSTATE_RETURNED
  | INVALID_SQL_STATEMENT_NAME
  | INVALID_TABLESAMPLE_ARGUMENT
  | INVALID_TABLESAMPLE_REPEAT
  | INVALID_TABLE_DEFINITION
  | INVALID_TEXT_REPRESENTATION
  | INVALID_TIME_ZONE_DISPLACEMENT_VALUE
  | INVALID_TRANSACTION_INITIATION
  | INVALID_TRANSACTION_STATE
  | INVALID_TRANSACTION_TERMINATION
  | INVALID_USE_OF_ESCAPE_CHARACTER
  | INVALID_XML_COMMENT
  | INVALID_XML_CONTENT
  | INVALID_XML_DOCUMENT
  | INVALID_XML_PROCESSING_INSTRUCTION
  | IN_FAILED_SQL_TRANSACTION
  | IO_ERROR
  | LOCATOR_EXCEPTION
  | LOCK_FILE_EXISTS
  | LOCK_NOT_AVAILABLE
  | MODIFYING_SQL_DATA_NOT_PERMITTED
  | MOST_SPECIFIC_TYPE_MISMATCH
  | NAME_TOO_LONG
  | NONSTANDARD_USE_OF_ESCAPE_CHARACTER
  | NOT_AN_XML_DOCUMENT
  | NOT_NULL_VIOLATION
  | NO_ACTIVE_SQL_TRANSACTION
  | NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION
  | NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED
  | NO_DATA
  | NO_DATA_FOUND
  | NULL_VALUE_ELIMINATED_IN_SET_FUNCTION
  | NULL_VALUE_NOT_ALLOWED
  | NULL_VALUE_NO_INDICATOR_PARAMETER
  | NUMERIC_VALUE_OUT_OF_RANGE
  | OBJECT_IN_USE
  | OBJECT_NOT_IN_PREREQUISITE_STATE
  | OPERATOR_INTERVENTION
  | OUT_OF_MEMORY
  | PLPGSQL_ERROR
  | PRIVILEGE_NOT_GRANTED
  | PRIVILEGE_NOT_REVOKED
  | PROGRAM_LIMIT_EXCEEDED
  | PROHIBITED_SQL_STATEMENT_ATTEMPTED
  | PROTOCOL_VIOLATION
  | QUERY_CANCELED
  | RAISE_EXCEPTION
  | READING_SQL_DATA_NOT_PERMITTED
  | READ_ONLY_SQL_TRANSACTION
  | RESERVED_NAME
  | RESTRICT_VIOLATION
  | SAVEPOINT_EXCEPTION
  | SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED
  | SEQUENCE_GENERATOR_LIMIT_EXCEEDED
  | SERIALIZATION_FAILURE
  | SNAPSHOT_TOO_OLD
  | SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION
  | SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION
  | SQL_ROUTINE_EXCEPTION
  | SQL_STATEMENT_NOT_YET_COMPLETE
  | SRF_PROTOCOL_VIOLATED
  | STACKED_DIAGNOSTICS_ACCESSED_WITHOUT_ACTIVE_HANDLER
  | STATEMENT_COMPLETION_UNKNOWN
  | STATEMENT_TOO_COMPLEX
  | STRING_DATA_LENGTH_MISMATCH
  | STRING_DATA_RIGHT_TRUNCATION
  | SUBSTRING_ERROR
  | SUCCESSFUL_COMPLETION
  | SYNTAX_ERROR
  | SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION
  | SYSTEM_ERROR
  | TOO_MANY_ARGUMENTS
  | TOO_MANY_COLUMNS
  | TOO_MANY_CONNECTIONS
  | TOO_MANY_ROWS
  | TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION
  | TRANSACTION_RESOLUTION_UNKNOWN
  | TRANSACTION_ROLLBACK
  | TRIGGERED_ACTION_EXCEPTION
  | TRIGGERED_DATA_CHANGE_VIOLATION
  | TRIGGER_PROTOCOL_VIOLATED
  | TRIM_ERROR
  | UNDEFINED_COLUMN
  | UNDEFINED_FILE
  | UNDEFINED_FUNCTION
  | UNDEFINED_OBJECT
  | UNDEFINED_PARAMETER
  | UNDEFINED_TABLE
  | UNIQUE_VIOLATION
  | UNTERMINATED_C_STRING
  | UNTRANSLATABLE_CHARACTER
  | WARNING
  | WINDOWING_ERROR
  | WITH_CHECK_OPTION_VIOLATION
  | WRONG_OBJECT_TYPE
  | ZERO_LENGTH_CHARACTER_STRING

let to_string = function
  | ACTIVE_SQL_TRANSACTION -> "ACTIVE_SQL_TRANSACTION"
  | ADMIN_SHUTDOWN -> "ADMIN_SHUTDOWN"
  | AMBIGUOUS_ALIAS -> "AMBIGUOUS_ALIAS"
  | AMBIGUOUS_COLUMN -> "AMBIGUOUS_COLUMN"
  | AMBIGUOUS_FUNCTION -> "AMBIGUOUS_FUNCTION"
  | AMBIGUOUS_PARAMETER -> "AMBIGUOUS_PARAMETER"
  | ARRAY_SUBSCRIPT_ERROR -> "ARRAY_SUBSCRIPT_ERROR"
  | ASSERT_FAILURE -> "ASSERT_FAILURE"
  | BAD_COPY_FILE_FORMAT -> "BAD_COPY_FILE_FORMAT"
  | BRANCH_TRANSACTION_ALREADY_ACTIVE -> "BRANCH_TRANSACTION_ALREADY_ACTIVE"
  | CANNOT_COERCE -> "CANNOT_COERCE"
  | CANNOT_CONNECT_NOW -> "CANNOT_CONNECT_NOW"
  | CANT_CHANGE_RUNTIME_PARAM -> "CANT_CHANGE_RUNTIME_PARAM"
  | CARDINALITY_VIOLATION -> "CARDINALITY_VIOLATION"
  | CASE_NOT_FOUND -> "CASE_NOT_FOUND"
  | CHARACTER_NOT_IN_REPERTOIRE -> "CHARACTER_NOT_IN_REPERTOIRE"
  | CHECK_VIOLATION -> "CHECK_VIOLATION"
  | COLLATION_MISMATCH -> "COLLATION_MISMATCH"
  | CONFIGURATION_LIMIT_EXCEEDED -> "CONFIGURATION_LIMIT_EXCEEDED"
  | CONFIG_FILE_ERROR -> "CONFIG_FILE_ERROR"
  | CONNECTION_DOES_NOT_EXIST -> "CONNECTION_DOES_NOT_EXIST"
  | CONNECTION_EXCEPTION -> "CONNECTION_EXCEPTION"
  | CONNECTION_FAILURE -> "CONNECTION_FAILURE"
  | CONTAINING_SQL_NOT_PERMITTED -> "CONTAINING_SQL_NOT_PERMITTED"
  | CRASH_SHUTDOWN -> "CRASH_SHUTDOWN"
  | DATABASE_DROPPED -> "DATABASE_DROPPED"
  | DATATYPE_MISMATCH -> "DATATYPE_MISMATCH"
  | DATA_CORRUPTED -> "DATA_CORRUPTED"
  | DATA_EXCEPTION -> "DATA_EXCEPTION"
  | DATETIME_FIELD_OVERFLOW -> "DATETIME_FIELD_OVERFLOW"
  | DEADLOCK_DETECTED -> "DEADLOCK_DETECTED"
  | DEPENDENT_OBJECTS_STILL_EXIST -> "DEPENDENT_OBJECTS_STILL_EXIST"
  | DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST ->
      "DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST"
  | DEPRECATED_FEATURE -> "DEPRECATED_FEATURE"
  | DIAGNOSTICS_EXCEPTION -> "DIAGNOSTICS_EXCEPTION"
  | DISK_FULL -> "DISK_FULL"
  | DIVISION_BY_ZERO -> "DIVISION_BY_ZERO"
  | DUPLICATE_ALIAS -> "DUPLICATE_ALIAS"
  | DUPLICATE_COLUMN -> "DUPLICATE_COLUMN"
  | DUPLICATE_CURSOR -> "DUPLICATE_CURSOR"
  | DUPLICATE_DATABASE -> "DUPLICATE_DATABASE"
  | DUPLICATE_FILE -> "DUPLICATE_FILE"
  | DUPLICATE_FUNCTION -> "DUPLICATE_FUNCTION"
  | DUPLICATE_OBJECT -> "DUPLICATE_OBJECT"
  | DUPLICATE_PREPARED_STATEMENT -> "DUPLICATE_PREPARED_STATEMENT"
  | DUPLICATE_SCHEMA -> "DUPLICATE_SCHEMA"
  | DUPLICATE_TABLE -> "DUPLICATE_TABLE"
  | DYNAMIC_RESULT_SETS_RETURNED -> "DYNAMIC_RESULT_SETS_RETURNED"
  | ERROR_IN_ASSIGNMENT -> "ERROR_IN_ASSIGNMENT"
  | ESCAPE_CHARACTER_CONFLICT -> "ESCAPE_CHARACTER_CONFLICT"
  | EVENT_TRIGGER_PROTOCOL_VIOLATED -> "EVENT_TRIGGER_PROTOCOL_VIOLATED"
  | EXCLUSION_VIOLATION -> "EXCLUSION_VIOLATION"
  | EXTERNAL_ROUTINE_EXCEPTION -> "EXTERNAL_ROUTINE_EXCEPTION"
  | EXTERNAL_ROUTINE_INVOCATION_EXCEPTION ->
      "EXTERNAL_ROUTINE_INVOCATION_EXCEPTION"
  | FDW_COLUMN_NAME_NOT_FOUND -> "FDW_COLUMN_NAME_NOT_FOUND"
  | FDW_DYNAMIC_PARAMETER_VALUE_NEEDED -> "FDW_DYNAMIC_PARAMETER_VALUE_NEEDED"
  | FDW_ERROR -> "FDW_ERROR"
  | FDW_FUNCTION_SEQUENCE_ERROR -> "FDW_FUNCTION_SEQUENCE_ERROR"
  | FDW_INCONSISTENT_DESCRIPTOR_INFORMATION ->
      "FDW_INCONSISTENT_DESCRIPTOR_INFORMATION"
  | FDW_INVALID_ATTRIBUTE_VALUE -> "FDW_INVALID_ATTRIBUTE_VALUE"
  | FDW_INVALID_COLUMN_NAME -> "FDW_INVALID_COLUMN_NAME"
  | FDW_INVALID_COLUMN_NUMBER -> "FDW_INVALID_COLUMN_NUMBER"
  | FDW_INVALID_DATA_TYPE -> "FDW_INVALID_DATA_TYPE"
  | FDW_INVALID_DATA_TYPE_DESCRIPTORS -> "FDW_INVALID_DATA_TYPE_DESCRIPTORS"
  | FDW_INVALID_DESCRIPTOR_FIELD_IDENTIFIER ->
      "FDW_INVALID_DESCRIPTOR_FIELD_IDENTIFIER"
  | FDW_INVALID_HANDLE -> "FDW_INVALID_HANDLE"
  | FDW_INVALID_OPTION_INDEX -> "FDW_INVALID_OPTION_INDEX"
  | FDW_INVALID_OPTION_NAME -> "FDW_INVALID_OPTION_NAME"
  | FDW_INVALID_STRING_FORMAT -> "FDW_INVALID_STRING_FORMAT"
  | FDW_INVALID_STRING_LENGTH_OR_BUFFER_LENGTH ->
      "FDW_INVALID_STRING_LENGTH_OR_BUFFER_LENGTH"
  | FDW_INVALID_USE_OF_NULL_POINTER -> "FDW_INVALID_USE_OF_NULL_POINTER"
  | FDW_NO_SCHEMAS -> "FDW_NO_SCHEMAS"
  | FDW_OPTION_NAME_NOT_FOUND -> "FDW_OPTION_NAME_NOT_FOUND"
  | FDW_OUT_OF_MEMORY -> "FDW_OUT_OF_MEMORY"
  | FDW_REPLY_HANDLE -> "FDW_REPLY_HANDLE"
  | FDW_SCHEMA_NOT_FOUND -> "FDW_SCHEMA_NOT_FOUND"
  | FDW_TABLE_NOT_FOUND -> "FDW_TABLE_NOT_FOUND"
  | FDW_TOO_MANY_HANDLES -> "FDW_TOO_MANY_HANDLES"
  | FDW_UNABLE_TO_CREATE_EXECUTION -> "FDW_UNABLE_TO_CREATE_EXECUTION"
  | FDW_UNABLE_TO_CREATE_REPLY -> "FDW_UNABLE_TO_CREATE_REPLY"
  | FDW_UNABLE_TO_ESTABLISH_CONNECTION -> "FDW_UNABLE_TO_ESTABLISH_CONNECTION"
  | FEATURE_NOT_SUPPORTED -> "FEATURE_NOT_SUPPORTED"
  | FLOATING_POINT_EXCEPTION -> "FLOATING_POINT_EXCEPTION"
  | FOREIGN_KEY_VIOLATION -> "FOREIGN_KEY_VIOLATION"
  | FUNCTION_EXECUTED_NO_RETURN_STATEMENT ->
      "FUNCTION_EXECUTED_NO_RETURN_STATEMENT"
  | GENERATED_ALWAYS -> "GENERATED_ALWAYS"
  | GROUPING_ERROR -> "GROUPING_ERROR"
  | HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL ->
      "HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL"
  | IDLE_IN_TRANSACTION_SESSION_TIMEOUT -> "IDLE_IN_TRANSACTION_SESSION_TIMEOUT"
  | IMPLICIT_ZERO_BIT_PADDING -> "IMPLICIT_ZERO_BIT_PADDING"
  | INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION ->
      "INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION"
  | INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION ->
      "INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION"
  | INDETERMINATE_COLLATION -> "INDETERMINATE_COLLATION"
  | INDETERMINATE_DATATYPE -> "INDETERMINATE_DATATYPE"
  | INDEX_CORRUPTED -> "INDEX_CORRUPTED"
  | INDICATOR_OVERFLOW -> "INDICATOR_OVERFLOW"
  | INSUFFICIENT_PRIVILEGE -> "INSUFFICIENT_PRIVILEGE"
  | INSUFFICIENT_RESOURCES -> "INSUFFICIENT_RESOURCES"
  | INTEGRITY_CONSTRAINT_VIOLATION -> "INTEGRITY_CONSTRAINT_VIOLATION"
  | INTERNAL_ERROR -> "INTERNAL_ERROR"
  | INTERVAL_FIELD_OVERFLOW -> "INTERVAL_FIELD_OVERFLOW"
  | INVALID_ARGUMENT_FOR_LOGARITHM -> "INVALID_ARGUMENT_FOR_LOGARITHM"
  | INVALID_ARGUMENT_FOR_NTH_VALUE_FUNCTION ->
      "INVALID_ARGUMENT_FOR_NTH_VALUE_FUNCTION"
  | INVALID_ARGUMENT_FOR_NTILE_FUNCTION -> "INVALID_ARGUMENT_FOR_NTILE_FUNCTION"
  | INVALID_ARGUMENT_FOR_POWER_FUNCTION -> "INVALID_ARGUMENT_FOR_POWER_FUNCTION"
  | INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION ->
      "INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION"
  | INVALID_AUTHORIZATION_SPECIFICATION -> "INVALID_AUTHORIZATION_SPECIFICATION"
  | INVALID_BINARY_REPRESENTATION -> "INVALID_BINARY_REPRESENTATION"
  | INVALID_CATALOG_NAME -> "INVALID_CATALOG_NAME"
  | INVALID_CHARACTER_VALUE_FOR_CAST -> "INVALID_CHARACTER_VALUE_FOR_CAST"
  | INVALID_COLUMN_DEFINITION -> "INVALID_COLUMN_DEFINITION"
  | INVALID_COLUMN_REFERENCE -> "INVALID_COLUMN_REFERENCE"
  | INVALID_CURSOR_DEFINITION -> "INVALID_CURSOR_DEFINITION"
  | INVALID_CURSOR_NAME -> "INVALID_CURSOR_NAME"
  | INVALID_CURSOR_STATE -> "INVALID_CURSOR_STATE"
  | INVALID_DATABASE_DEFINITION -> "INVALID_DATABASE_DEFINITION"
  | INVALID_DATETIME_FORMAT -> "INVALID_DATETIME_FORMAT"
  | INVALID_ESCAPE_CHARACTER -> "INVALID_ESCAPE_CHARACTER"
  | INVALID_ESCAPE_OCTET -> "INVALID_ESCAPE_OCTET"
  | INVALID_ESCAPE_SEQUENCE -> "INVALID_ESCAPE_SEQUENCE"
  | INVALID_FOREIGN_KEY -> "INVALID_FOREIGN_KEY"
  | INVALID_FUNCTION_DEFINITION -> "INVALID_FUNCTION_DEFINITION"
  | INVALID_GRANTOR -> "INVALID_GRANTOR"
  | INVALID_GRANT_OPERATION -> "INVALID_GRANT_OPERATION"
  | INVALID_INDICATOR_PARAMETER_VALUE -> "INVALID_INDICATOR_PARAMETER_VALUE"
  | INVALID_LOCATOR_SPECIFICATION -> "INVALID_LOCATOR_SPECIFICATION"
  | INVALID_NAME -> "INVALID_NAME"
  | INVALID_OBJECT_DEFINITION -> "INVALID_OBJECT_DEFINITION"
  | INVALID_PARAMETER_VALUE -> "INVALID_PARAMETER_VALUE"
  | INVALID_PASSWORD -> "INVALID_PASSWORD"
  | INVALID_PREPARED_STATEMENT_DEFINITION ->
      "INVALID_PREPARED_STATEMENT_DEFINITION"
  | INVALID_RECURSION -> "INVALID_RECURSION"
  | INVALID_REGULAR_EXPRESSION -> "INVALID_REGULAR_EXPRESSION"
  | INVALID_ROLE_SPECIFICATION -> "INVALID_ROLE_SPECIFICATION"
  | INVALID_ROW_COUNT_IN_LIMIT_CLAUSE -> "INVALID_ROW_COUNT_IN_LIMIT_CLAUSE"
  | INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE ->
      "INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE"
  | INVALID_SAVEPOINT_SPECIFICATION -> "INVALID_SAVEPOINT_SPECIFICATION"
  | INVALID_SCHEMA_DEFINITION -> "INVALID_SCHEMA_DEFINITION"
  | INVALID_SCHEMA_NAME -> "INVALID_SCHEMA_NAME"
  | INVALID_SQLSTATE_RETURNED -> "INVALID_SQLSTATE_RETURNED"
  | INVALID_SQL_STATEMENT_NAME -> "INVALID_SQL_STATEMENT_NAME"
  | INVALID_TABLESAMPLE_ARGUMENT -> "INVALID_TABLESAMPLE_ARGUMENT"
  | INVALID_TABLESAMPLE_REPEAT -> "INVALID_TABLESAMPLE_REPEAT"
  | INVALID_TABLE_DEFINITION -> "INVALID_TABLE_DEFINITION"
  | INVALID_TEXT_REPRESENTATION -> "INVALID_TEXT_REPRESENTATION"
  | INVALID_TIME_ZONE_DISPLACEMENT_VALUE ->
      "INVALID_TIME_ZONE_DISPLACEMENT_VALUE"
  | INVALID_TRANSACTION_INITIATION -> "INVALID_TRANSACTION_INITIATION"
  | INVALID_TRANSACTION_STATE -> "INVALID_TRANSACTION_STATE"
  | INVALID_TRANSACTION_TERMINATION -> "INVALID_TRANSACTION_TERMINATION"
  | INVALID_USE_OF_ESCAPE_CHARACTER -> "INVALID_USE_OF_ESCAPE_CHARACTER"
  | INVALID_XML_COMMENT -> "INVALID_XML_COMMENT"
  | INVALID_XML_CONTENT -> "INVALID_XML_CONTENT"
  | INVALID_XML_DOCUMENT -> "INVALID_XML_DOCUMENT"
  | INVALID_XML_PROCESSING_INSTRUCTION -> "INVALID_XML_PROCESSING_INSTRUCTION"
  | IN_FAILED_SQL_TRANSACTION -> "IN_FAILED_SQL_TRANSACTION"
  | IO_ERROR -> "IO_ERROR"
  | LOCATOR_EXCEPTION -> "LOCATOR_EXCEPTION"
  | LOCK_FILE_EXISTS -> "LOCK_FILE_EXISTS"
  | LOCK_NOT_AVAILABLE -> "LOCK_NOT_AVAILABLE"
  | MODIFYING_SQL_DATA_NOT_PERMITTED -> "MODIFYING_SQL_DATA_NOT_PERMITTED"
  | MOST_SPECIFIC_TYPE_MISMATCH -> "MOST_SPECIFIC_TYPE_MISMATCH"
  | NAME_TOO_LONG -> "NAME_TOO_LONG"
  | NONSTANDARD_USE_OF_ESCAPE_CHARACTER -> "NONSTANDARD_USE_OF_ESCAPE_CHARACTER"
  | NOT_AN_XML_DOCUMENT -> "NOT_AN_XML_DOCUMENT"
  | NOT_NULL_VIOLATION -> "NOT_NULL_VIOLATION"
  | NO_ACTIVE_SQL_TRANSACTION -> "NO_ACTIVE_SQL_TRANSACTION"
  | NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION ->
      "NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION"
  | NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED ->
      "NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED"
  | NO_DATA -> "NO_DATA"
  | NO_DATA_FOUND -> "NO_DATA_FOUND"
  | NULL_VALUE_ELIMINATED_IN_SET_FUNCTION ->
      "NULL_VALUE_ELIMINATED_IN_SET_FUNCTION"
  | NULL_VALUE_NOT_ALLOWED -> "NULL_VALUE_NOT_ALLOWED"
  | NULL_VALUE_NO_INDICATOR_PARAMETER -> "NULL_VALUE_NO_INDICATOR_PARAMETER"
  | NUMERIC_VALUE_OUT_OF_RANGE -> "NUMERIC_VALUE_OUT_OF_RANGE"
  | OBJECT_IN_USE -> "OBJECT_IN_USE"
  | OBJECT_NOT_IN_PREREQUISITE_STATE -> "OBJECT_NOT_IN_PREREQUISITE_STATE"
  | OPERATOR_INTERVENTION -> "OPERATOR_INTERVENTION"
  | OUT_OF_MEMORY -> "OUT_OF_MEMORY"
  | PLPGSQL_ERROR -> "PLPGSQL_ERROR"
  | PRIVILEGE_NOT_GRANTED -> "PRIVILEGE_NOT_GRANTED"
  | PRIVILEGE_NOT_REVOKED -> "PRIVILEGE_NOT_REVOKED"
  | PROGRAM_LIMIT_EXCEEDED -> "PROGRAM_LIMIT_EXCEEDED"
  | PROHIBITED_SQL_STATEMENT_ATTEMPTED -> "PROHIBITED_SQL_STATEMENT_ATTEMPTED"
  | PROTOCOL_VIOLATION -> "PROTOCOL_VIOLATION"
  | QUERY_CANCELED -> "QUERY_CANCELED"
  | RAISE_EXCEPTION -> "RAISE_EXCEPTION"
  | READING_SQL_DATA_NOT_PERMITTED -> "READING_SQL_DATA_NOT_PERMITTED"
  | READ_ONLY_SQL_TRANSACTION -> "READ_ONLY_SQL_TRANSACTION"
  | RESERVED_NAME -> "RESERVED_NAME"
  | RESTRICT_VIOLATION -> "RESTRICT_VIOLATION"
  | SAVEPOINT_EXCEPTION -> "SAVEPOINT_EXCEPTION"
  | SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED ->
      "SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED"
  | SEQUENCE_GENERATOR_LIMIT_EXCEEDED -> "SEQUENCE_GENERATOR_LIMIT_EXCEEDED"
  | SERIALIZATION_FAILURE -> "SERIALIZATION_FAILURE"
  | SNAPSHOT_TOO_OLD -> "SNAPSHOT_TOO_OLD"
  | SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION ->
      "SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION"
  | SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION ->
      "SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION"
  | SQL_ROUTINE_EXCEPTION -> "SQL_ROUTINE_EXCEPTION"
  | SQL_STATEMENT_NOT_YET_COMPLETE -> "SQL_STATEMENT_NOT_YET_COMPLETE"
  | SRF_PROTOCOL_VIOLATED -> "SRF_PROTOCOL_VIOLATED"
  | STACKED_DIAGNOSTICS_ACCESSED_WITHOUT_ACTIVE_HANDLER ->
      "STACKED_DIAGNOSTICS_ACCESSED_WITHOUT_ACTIVE_HANDLER"
  | STATEMENT_COMPLETION_UNKNOWN -> "STATEMENT_COMPLETION_UNKNOWN"
  | STATEMENT_TOO_COMPLEX -> "STATEMENT_TOO_COMPLEX"
  | STRING_DATA_LENGTH_MISMATCH -> "STRING_DATA_LENGTH_MISMATCH"
  | STRING_DATA_RIGHT_TRUNCATION -> "STRING_DATA_RIGHT_TRUNCATION"
  | SUBSTRING_ERROR -> "SUBSTRING_ERROR"
  | SUCCESSFUL_COMPLETION -> "SUCCESSFUL_COMPLETION"
  | SYNTAX_ERROR -> "SYNTAX_ERROR"
  | SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION ->
      "SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION"
  | SYSTEM_ERROR -> "SYSTEM_ERROR"
  | TOO_MANY_ARGUMENTS -> "TOO_MANY_ARGUMENTS"
  | TOO_MANY_COLUMNS -> "TOO_MANY_COLUMNS"
  | TOO_MANY_CONNECTIONS -> "TOO_MANY_CONNECTIONS"
  | TOO_MANY_ROWS -> "TOO_MANY_ROWS"
  | TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION ->
      "TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION"
  | TRANSACTION_RESOLUTION_UNKNOWN -> "TRANSACTION_RESOLUTION_UNKNOWN"
  | TRANSACTION_ROLLBACK -> "TRANSACTION_ROLLBACK"
  | TRIGGERED_ACTION_EXCEPTION -> "TRIGGERED_ACTION_EXCEPTION"
  | TRIGGERED_DATA_CHANGE_VIOLATION -> "TRIGGERED_DATA_CHANGE_VIOLATION"
  | TRIGGER_PROTOCOL_VIOLATED -> "TRIGGER_PROTOCOL_VIOLATED"
  | TRIM_ERROR -> "TRIM_ERROR"
  | UNDEFINED_COLUMN -> "UNDEFINED_COLUMN"
  | UNDEFINED_FILE -> "UNDEFINED_FILE"
  | UNDEFINED_FUNCTION -> "UNDEFINED_FUNCTION"
  | UNDEFINED_OBJECT -> "UNDEFINED_OBJECT"
  | UNDEFINED_PARAMETER -> "UNDEFINED_PARAMETER"
  | UNDEFINED_TABLE -> "UNDEFINED_TABLE"
  | UNIQUE_VIOLATION -> "UNIQUE_VIOLATION"
  | UNTERMINATED_C_STRING -> "UNTERMINATED_C_STRING"
  | UNTRANSLATABLE_CHARACTER -> "UNTRANSLATABLE_CHARACTER"
  | WARNING -> "WARNING"
  | WINDOWING_ERROR -> "WINDOWING_ERROR"
  | WITH_CHECK_OPTION_VIOLATION -> "WITH_CHECK_OPTION_VIOLATION"
  | WRONG_OBJECT_TYPE -> "WRONG_OBJECT_TYPE"
  | ZERO_LENGTH_CHARACTER_STRING -> "ZERO_LENGTH_CHARACTER_STRING"

let of_sqlstate = function
  | "00000" -> SUCCESSFUL_COMPLETION
  | "01000" -> WARNING
  | "0100C" -> DYNAMIC_RESULT_SETS_RETURNED
  | "01008" -> IMPLICIT_ZERO_BIT_PADDING
  | "01003" -> NULL_VALUE_ELIMINATED_IN_SET_FUNCTION
  | "01007" -> PRIVILEGE_NOT_GRANTED
  | "01006" -> PRIVILEGE_NOT_REVOKED
  | "01004" -> STRING_DATA_RIGHT_TRUNCATION
  | "01P01" -> DEPRECATED_FEATURE
  | "02000" -> NO_DATA
  | "02001" -> NO_ADDITIONAL_DYNAMIC_RESULT_SETS_RETURNED
  | "03000" -> SQL_STATEMENT_NOT_YET_COMPLETE
  | "08000" -> CONNECTION_EXCEPTION
  | "08003" -> CONNECTION_DOES_NOT_EXIST
  | "08006" -> CONNECTION_FAILURE
  | "08001" -> SQLCLIENT_UNABLE_TO_ESTABLISH_SQLCONNECTION
  | "08004" -> SQLSERVER_REJECTED_ESTABLISHMENT_OF_SQLCONNECTION
  | "08007" -> TRANSACTION_RESOLUTION_UNKNOWN
  | "08P01" -> PROTOCOL_VIOLATION
  | "09000" -> TRIGGERED_ACTION_EXCEPTION
  | "0A000" -> FEATURE_NOT_SUPPORTED
  | "0B000" -> INVALID_TRANSACTION_INITIATION
  | "0F000" -> LOCATOR_EXCEPTION
  | "0F001" -> INVALID_LOCATOR_SPECIFICATION
  | "0L000" -> INVALID_GRANTOR
  | "0LP01" -> INVALID_GRANT_OPERATION
  | "0P000" -> INVALID_ROLE_SPECIFICATION
  | "0Z000" -> DIAGNOSTICS_EXCEPTION
  | "0Z002" -> STACKED_DIAGNOSTICS_ACCESSED_WITHOUT_ACTIVE_HANDLER
  | "20000" -> CASE_NOT_FOUND
  | "21000" -> CARDINALITY_VIOLATION
  | "22000" -> DATA_EXCEPTION
  | "2202E" -> ARRAY_SUBSCRIPT_ERROR
  | "22021" -> CHARACTER_NOT_IN_REPERTOIRE
  | "22008" -> DATETIME_FIELD_OVERFLOW
  | "22012" -> DIVISION_BY_ZERO
  | "22005" -> ERROR_IN_ASSIGNMENT
  | "2200B" -> ESCAPE_CHARACTER_CONFLICT
  | "22022" -> INDICATOR_OVERFLOW
  | "22015" -> INTERVAL_FIELD_OVERFLOW
  | "2201E" -> INVALID_ARGUMENT_FOR_LOGARITHM
  | "22014" -> INVALID_ARGUMENT_FOR_NTILE_FUNCTION
  | "22016" -> INVALID_ARGUMENT_FOR_NTH_VALUE_FUNCTION
  | "2201F" -> INVALID_ARGUMENT_FOR_POWER_FUNCTION
  | "2201G" -> INVALID_ARGUMENT_FOR_WIDTH_BUCKET_FUNCTION
  | "22018" -> INVALID_CHARACTER_VALUE_FOR_CAST
  | "22007" -> INVALID_DATETIME_FORMAT
  | "22019" -> INVALID_ESCAPE_CHARACTER
  | "2200D" -> INVALID_ESCAPE_OCTET
  | "22025" -> INVALID_ESCAPE_SEQUENCE
  | "22P06" -> NONSTANDARD_USE_OF_ESCAPE_CHARACTER
  | "22010" -> INVALID_INDICATOR_PARAMETER_VALUE
  | "22023" -> INVALID_PARAMETER_VALUE
  | "2201B" -> INVALID_REGULAR_EXPRESSION
  | "2201W" -> INVALID_ROW_COUNT_IN_LIMIT_CLAUSE
  | "2201X" -> INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE
  | "2202H" -> INVALID_TABLESAMPLE_ARGUMENT
  | "2202G" -> INVALID_TABLESAMPLE_REPEAT
  | "22009" -> INVALID_TIME_ZONE_DISPLACEMENT_VALUE
  | "2200C" -> INVALID_USE_OF_ESCAPE_CHARACTER
  | "2200G" -> MOST_SPECIFIC_TYPE_MISMATCH
  | "22004" -> NULL_VALUE_NOT_ALLOWED
  | "22002" -> NULL_VALUE_NO_INDICATOR_PARAMETER
  | "22003" -> NUMERIC_VALUE_OUT_OF_RANGE
  | "2200H" -> SEQUENCE_GENERATOR_LIMIT_EXCEEDED
  | "22026" -> STRING_DATA_LENGTH_MISMATCH
  | "22001" -> STRING_DATA_RIGHT_TRUNCATION
  | "22011" -> SUBSTRING_ERROR
  | "22027" -> TRIM_ERROR
  | "22024" -> UNTERMINATED_C_STRING
  | "2200F" -> ZERO_LENGTH_CHARACTER_STRING
  | "22P01" -> FLOATING_POINT_EXCEPTION
  | "22P02" -> INVALID_TEXT_REPRESENTATION
  | "22P03" -> INVALID_BINARY_REPRESENTATION
  | "22P04" -> BAD_COPY_FILE_FORMAT
  | "22P05" -> UNTRANSLATABLE_CHARACTER
  | "2200L" -> NOT_AN_XML_DOCUMENT
  | "2200M" -> INVALID_XML_DOCUMENT
  | "2200N" -> INVALID_XML_CONTENT
  | "2200S" -> INVALID_XML_COMMENT
  | "2200T" -> INVALID_XML_PROCESSING_INSTRUCTION
  | "23000" -> INTEGRITY_CONSTRAINT_VIOLATION
  | "23001" -> RESTRICT_VIOLATION
  | "23502" -> NOT_NULL_VIOLATION
  | "23503" -> FOREIGN_KEY_VIOLATION
  | "23505" -> UNIQUE_VIOLATION
  | "23514" -> CHECK_VIOLATION
  | "23P01" -> EXCLUSION_VIOLATION
  | "24000" -> INVALID_CURSOR_STATE
  | "25000" -> INVALID_TRANSACTION_STATE
  | "25001" -> ACTIVE_SQL_TRANSACTION
  | "25002" -> BRANCH_TRANSACTION_ALREADY_ACTIVE
  | "25008" -> HELD_CURSOR_REQUIRES_SAME_ISOLATION_LEVEL
  | "25003" -> INAPPROPRIATE_ACCESS_MODE_FOR_BRANCH_TRANSACTION
  | "25004" -> INAPPROPRIATE_ISOLATION_LEVEL_FOR_BRANCH_TRANSACTION
  | "25005" -> NO_ACTIVE_SQL_TRANSACTION_FOR_BRANCH_TRANSACTION
  | "25006" -> READ_ONLY_SQL_TRANSACTION
  | "25007" -> SCHEMA_AND_DATA_STATEMENT_MIXING_NOT_SUPPORTED
  | "25P01" -> NO_ACTIVE_SQL_TRANSACTION
  | "25P02" -> IN_FAILED_SQL_TRANSACTION
  | "25P03" -> IDLE_IN_TRANSACTION_SESSION_TIMEOUT
  | "26000" -> INVALID_SQL_STATEMENT_NAME
  | "27000" -> TRIGGERED_DATA_CHANGE_VIOLATION
  | "28000" -> INVALID_AUTHORIZATION_SPECIFICATION
  | "28P01" -> INVALID_PASSWORD
  | "2B000" -> DEPENDENT_PRIVILEGE_DESCRIPTORS_STILL_EXIST
  | "2BP01" -> DEPENDENT_OBJECTS_STILL_EXIST
  | "2D000" -> INVALID_TRANSACTION_TERMINATION
  | "2F000" -> SQL_ROUTINE_EXCEPTION
  | "2F005" -> FUNCTION_EXECUTED_NO_RETURN_STATEMENT
  | "2F002" -> MODIFYING_SQL_DATA_NOT_PERMITTED
  | "2F003" -> PROHIBITED_SQL_STATEMENT_ATTEMPTED
  | "2F004" -> READING_SQL_DATA_NOT_PERMITTED
  | "34000" -> INVALID_CURSOR_NAME
  | "38000" -> EXTERNAL_ROUTINE_EXCEPTION
  | "38001" -> CONTAINING_SQL_NOT_PERMITTED
  | "38002" -> MODIFYING_SQL_DATA_NOT_PERMITTED
  | "38003" -> PROHIBITED_SQL_STATEMENT_ATTEMPTED
  | "38004" -> READING_SQL_DATA_NOT_PERMITTED
  | "39000" -> EXTERNAL_ROUTINE_INVOCATION_EXCEPTION
  | "39001" -> INVALID_SQLSTATE_RETURNED
  | "39004" -> NULL_VALUE_NOT_ALLOWED
  | "39P01" -> TRIGGER_PROTOCOL_VIOLATED
  | "39P02" -> SRF_PROTOCOL_VIOLATED
  | "39P03" -> EVENT_TRIGGER_PROTOCOL_VIOLATED
  | "3B000" -> SAVEPOINT_EXCEPTION
  | "3B001" -> INVALID_SAVEPOINT_SPECIFICATION
  | "3D000" -> INVALID_CATALOG_NAME
  | "3F000" -> INVALID_SCHEMA_NAME
  | "40000" -> TRANSACTION_ROLLBACK
  | "40002" -> TRANSACTION_INTEGRITY_CONSTRAINT_VIOLATION
  | "40001" -> SERIALIZATION_FAILURE
  | "40003" -> STATEMENT_COMPLETION_UNKNOWN
  | "40P01" -> DEADLOCK_DETECTED
  | "42000" -> SYNTAX_ERROR_OR_ACCESS_RULE_VIOLATION
  | "42601" -> SYNTAX_ERROR
  | "42501" -> INSUFFICIENT_PRIVILEGE
  | "42846" -> CANNOT_COERCE
  | "42803" -> GROUPING_ERROR
  | "42P20" -> WINDOWING_ERROR
  | "42P19" -> INVALID_RECURSION
  | "42830" -> INVALID_FOREIGN_KEY
  | "42602" -> INVALID_NAME
  | "42622" -> NAME_TOO_LONG
  | "42939" -> RESERVED_NAME
  | "42804" -> DATATYPE_MISMATCH
  | "42P18" -> INDETERMINATE_DATATYPE
  | "42P21" -> COLLATION_MISMATCH
  | "42P22" -> INDETERMINATE_COLLATION
  | "42809" -> WRONG_OBJECT_TYPE
  | "428C9" -> GENERATED_ALWAYS
  | "42703" -> UNDEFINED_COLUMN
  | "42883" -> UNDEFINED_FUNCTION
  | "42P01" -> UNDEFINED_TABLE
  | "42P02" -> UNDEFINED_PARAMETER
  | "42704" -> UNDEFINED_OBJECT
  | "42701" -> DUPLICATE_COLUMN
  | "42P03" -> DUPLICATE_CURSOR
  | "42P04" -> DUPLICATE_DATABASE
  | "42723" -> DUPLICATE_FUNCTION
  | "42P05" -> DUPLICATE_PREPARED_STATEMENT
  | "42P06" -> DUPLICATE_SCHEMA
  | "42P07" -> DUPLICATE_TABLE
  | "42712" -> DUPLICATE_ALIAS
  | "42710" -> DUPLICATE_OBJECT
  | "42702" -> AMBIGUOUS_COLUMN
  | "42725" -> AMBIGUOUS_FUNCTION
  | "42P08" -> AMBIGUOUS_PARAMETER
  | "42P09" -> AMBIGUOUS_ALIAS
  | "42P10" -> INVALID_COLUMN_REFERENCE
  | "42611" -> INVALID_COLUMN_DEFINITION
  | "42P11" -> INVALID_CURSOR_DEFINITION
  | "42P12" -> INVALID_DATABASE_DEFINITION
  | "42P13" -> INVALID_FUNCTION_DEFINITION
  | "42P14" -> INVALID_PREPARED_STATEMENT_DEFINITION
  | "42P15" -> INVALID_SCHEMA_DEFINITION
  | "42P16" -> INVALID_TABLE_DEFINITION
  | "42P17" -> INVALID_OBJECT_DEFINITION
  | "44000" -> WITH_CHECK_OPTION_VIOLATION
  | "53000" -> INSUFFICIENT_RESOURCES
  | "53100" -> DISK_FULL
  | "53200" -> OUT_OF_MEMORY
  | "53300" -> TOO_MANY_CONNECTIONS
  | "53400" -> CONFIGURATION_LIMIT_EXCEEDED
  | "54000" -> PROGRAM_LIMIT_EXCEEDED
  | "54001" -> STATEMENT_TOO_COMPLEX
  | "54011" -> TOO_MANY_COLUMNS
  | "54023" -> TOO_MANY_ARGUMENTS
  | "55000" -> OBJECT_NOT_IN_PREREQUISITE_STATE
  | "55006" -> OBJECT_IN_USE
  | "55P02" -> CANT_CHANGE_RUNTIME_PARAM
  | "55P03" -> LOCK_NOT_AVAILABLE
  | "57000" -> OPERATOR_INTERVENTION
  | "57014" -> QUERY_CANCELED
  | "57P01" -> ADMIN_SHUTDOWN
  | "57P02" -> CRASH_SHUTDOWN
  | "57P03" -> CANNOT_CONNECT_NOW
  | "57P04" -> DATABASE_DROPPED
  | "58000" -> SYSTEM_ERROR
  | "58030" -> IO_ERROR
  | "58P01" -> UNDEFINED_FILE
  | "58P02" -> DUPLICATE_FILE
  | "72000" -> SNAPSHOT_TOO_OLD
  | "F0000" -> CONFIG_FILE_ERROR
  | "F0001" -> LOCK_FILE_EXISTS
  | "HV000" -> FDW_ERROR
  | "HV005" -> FDW_COLUMN_NAME_NOT_FOUND
  | "HV002" -> FDW_DYNAMIC_PARAMETER_VALUE_NEEDED
  | "HV010" -> FDW_FUNCTION_SEQUENCE_ERROR
  | "HV021" -> FDW_INCONSISTENT_DESCRIPTOR_INFORMATION
  | "HV024" -> FDW_INVALID_ATTRIBUTE_VALUE
  | "HV007" -> FDW_INVALID_COLUMN_NAME
  | "HV008" -> FDW_INVALID_COLUMN_NUMBER
  | "HV004" -> FDW_INVALID_DATA_TYPE
  | "HV006" -> FDW_INVALID_DATA_TYPE_DESCRIPTORS
  | "HV091" -> FDW_INVALID_DESCRIPTOR_FIELD_IDENTIFIER
  | "HV00B" -> FDW_INVALID_HANDLE
  | "HV00C" -> FDW_INVALID_OPTION_INDEX
  | "HV00D" -> FDW_INVALID_OPTION_NAME
  | "HV090" -> FDW_INVALID_STRING_LENGTH_OR_BUFFER_LENGTH
  | "HV00A" -> FDW_INVALID_STRING_FORMAT
  | "HV009" -> FDW_INVALID_USE_OF_NULL_POINTER
  | "HV014" -> FDW_TOO_MANY_HANDLES
  | "HV001" -> FDW_OUT_OF_MEMORY
  | "HV00P" -> FDW_NO_SCHEMAS
  | "HV00J" -> FDW_OPTION_NAME_NOT_FOUND
  | "HV00K" -> FDW_REPLY_HANDLE
  | "HV00Q" -> FDW_SCHEMA_NOT_FOUND
  | "HV00R" -> FDW_TABLE_NOT_FOUND
  | "HV00L" -> FDW_UNABLE_TO_CREATE_EXECUTION
  | "HV00M" -> FDW_UNABLE_TO_CREATE_REPLY
  | "HV00N" -> FDW_UNABLE_TO_ESTABLISH_CONNECTION
  | "P0000" -> PLPGSQL_ERROR
  | "P0001" -> RAISE_EXCEPTION
  | "P0002" -> NO_DATA_FOUND
  | "P0003" -> TOO_MANY_ROWS
  | "P0004" -> ASSERT_FAILURE
  | "XX000" -> INTERNAL_ERROR
  | "XX001" -> DATA_CORRUPTED
  | "XX002" -> INDEX_CORRUPTED
  | error_code -> raise (Failure ("Unknown SQLSTATE error code: " ^ error_code))
