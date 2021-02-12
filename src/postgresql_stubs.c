/*
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
*/

#if __GNUC__ >= 3
# define __unused __attribute__ ((unused))
#else
# define __unused
#endif

#if PG_OCAML_MAJOR_VERSION > 8 \
    || ( PG_OCAML_MAJOR_VERSION >= 8 && PG_OCAML_MINOR_VERSION >= 2)
# define PG_OCAML_8_2
#endif
#if PG_OCAML_MAJOR_VERSION > 9 \
    || ( PG_OCAML_MAJOR_VERSION >= 9 && PG_OCAML_MINOR_VERSION >= 2)
# define PG_OCAML_9_2
#endif

#include <string.h>
#include <ctype.h>
#include <stdbool.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include <libpq-fe.h>
#include <libpq/libpq-fs.h>

#define BOOLOID                   16
#define BYTEAOID                  17
#define CHAROID                   18
#define NAMEOID                   19
#define INT8OID                   20
#define INT2OID                   21
#define INT2VECTOROID             22
#define INT4OID                   23
#define REGPROCOID                24
#define TEXTOID                   25
#define OIDOID                    26
#define TIDOID                    27
#define XIDOID                    28
#define CIDOID                    29
#define OIDVECTOROID              30
#define JSONOID                  114
#define POINTOID                 600
#define LSEGOID                  601
#define PATHOID                  602
#define BOXOID                   603
#define POLYGONOID               604
#define LINEOID                  628
#define FLOAT4OID                700
#define FLOAT8OID                701
#define ABSTIMEOID               702
#define RELTIMEOID               703
#define TINTERVALOID             704
#define UNKNOWNOID               705
#define CIRCLEOID                718
#define CASHOID                  790
#define MACADDROID               829
#define INETOID                  869
#define CIDROID                  650
#define ACLITEMOID              1033
#define BPCHAROID               1042
#define VARCHAROID              1043
#define DATEOID                 1082
#define TIMEOID                 1083
#define TIMESTAMPOID            1114
#define TIMESTAMPTZOID          1184
#define INTERVALOID             1186
#define TIMETZOID               1266
#define BITOID                  1560
#define VARBITOID               1562
#define NUMERICOID              1700
#define REFCURSOROID            1790
#define REGPROCEDUREOID         2202
#define REGOPEROID              2203
#define REGOPERATOROID          2204
#define REGCLASSOID             2205
#define REGTYPEOID              2206
#define RECORDOID               2249
#define CSTRINGOID              2275
#define ANYOID                  2276
#define ANYARRAYOID             2277
#define VOIDOID                 2278
#define TRIGGEROID              2279
#define LANGUAGE_HANDLEROID     2280
#define INTERNALOID             2281
#define OPAQUEOID               2282
#define ANYELEMENTOID           2283
#define JSONBOID                3802

static value v_None = Val_int(0);

static inline value make_some(value v)
{
  CAMLparam1(v);
  value v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = v;
  CAMLreturn(v_res);
}

/* Cache for OCaml-values */
static value v_empty_string = Val_unit;
static const value *v_exc_Oid = NULL;  /* Exception [Oid] */
static const value *v_null_param = NULL;

CAMLprim value PQocaml_init(value __unused v_unit)
{
  v_empty_string = caml_alloc_string(0);
  caml_register_generational_global_root(&v_empty_string);
  v_exc_Oid = caml_named_value("Postgresql.Oid");
  v_null_param = caml_named_value("Postgresql.null");
  return Val_unit;
}


/* Conversion functions */

static int oid_tbl[] = {
  BOOLOID, BYTEAOID, CHAROID, NAMEOID, INT8OID, INT2OID, INT2VECTOROID,
  INT4OID, REGPROCOID, TEXTOID, OIDOID, TIDOID, XIDOID, CIDOID,
  OIDVECTOROID, JSONOID, POINTOID, LSEGOID, PATHOID, BOXOID, POLYGONOID,
  LINEOID, FLOAT4OID, FLOAT8OID, ABSTIMEOID, RELTIMEOID, TINTERVALOID,
  UNKNOWNOID, CIRCLEOID, CASHOID, MACADDROID, INETOID, CIDROID, ACLITEMOID,
  BPCHAROID, VARCHAROID, DATEOID, TIMEOID, TIMESTAMPOID, TIMESTAMPTZOID,
  INTERVALOID, TIMETZOID, BITOID, VARBITOID, NUMERICOID, REFCURSOROID,
  REGPROCEDUREOID, REGOPEROID, REGOPERATOROID, REGCLASSOID, REGTYPEOID,
  RECORDOID, CSTRINGOID, ANYOID, ANYARRAYOID, VOIDOID, TRIGGEROID,
  LANGUAGE_HANDLEROID, INTERNALOID, OPAQUEOID, ANYELEMENTOID, JSONBOID
};

CAMLprim value ftype_of_oid_stub(intnat oid)
{
  int *p = oid_tbl;
  int *last = oid_tbl + sizeof(oid_tbl)/sizeof(oid_tbl[0]);
  while (p != last && *p != oid) p++;
  if (p == last) caml_raise_with_arg(*v_exc_Oid, Val_int(oid));
  return Val_int(p - oid_tbl);
}

CAMLprim value ftype_of_oid_stub_bc(value v_oid)
{ return ftype_of_oid_stub(Int_val(v_oid)); }

CAMLprim intnat oid_of_ftype_stub(value v_ftype)
{ return oid_tbl[Int_val(v_ftype)]; }

CAMLprim value oid_of_ftype_stub_bc(value v_ftype)
{ return Val_int(oid_of_ftype_stub(v_ftype)); }

/* Error field conversions */

#define PG_DIAG_SEVERITY              'S'
#define PG_DIAG_SEVERITY_NONLOCALIZED 'V'
#define PG_DIAG_SQLSTATE              'C'
#define PG_DIAG_MESSAGE_PRIMARY       'M'
#define PG_DIAG_MESSAGE_DETAIL        'D'
#define PG_DIAG_MESSAGE_HINT          'H'
#define PG_DIAG_STATEMENT_POSITION    'P'
#define PG_DIAG_INTERNAL_POSITION     'p'
#define PG_DIAG_INTERNAL_QUERY        'q'
#define PG_DIAG_CONTEXT               'W'
#define PG_DIAG_SCHEMA_NAME           's'
#define PG_DIAG_TABLE_NAME            't'
#define PG_DIAG_COLUMN_NAME           'c'
#define PG_DIAG_DATATYPE_NAME         'd'
#define PG_DIAG_CONSTRAINT_NAME       'n'
#define PG_DIAG_SOURCE_FILE           'F'
#define PG_DIAG_SOURCE_LINE           'L'
#define PG_DIAG_SOURCE_FUNCTION       'R'

static char error_field_tbl[] = {
  PG_DIAG_SEVERITY, PG_DIAG_SEVERITY_NONLOCALIZED, PG_DIAG_SQLSTATE,
  PG_DIAG_MESSAGE_PRIMARY, PG_DIAG_MESSAGE_DETAIL, PG_DIAG_MESSAGE_HINT,
  PG_DIAG_STATEMENT_POSITION, PG_DIAG_INTERNAL_POSITION, PG_DIAG_INTERNAL_QUERY,
  PG_DIAG_CONTEXT, PG_DIAG_SCHEMA_NAME, PG_DIAG_TABLE_NAME, PG_DIAG_COLUMN_NAME,
  PG_DIAG_DATATYPE_NAME, PG_DIAG_CONSTRAINT_NAME, PG_DIAG_SOURCE_FILE,
  PG_DIAG_SOURCE_LINE, PG_DIAG_SOURCE_FUNCTION
};

/* Management of notice_processor callbacks */

/* One must me careful with notice processors: the callback
   can be called after the death of the connection if
   a living PGresult was made from the connection. */

typedef struct {
  int cnt;  /* reference counter; number of connections (at most 1) plus
               results attached to the callback */
  value v_cb;  /* the callback itself, registered as a global root */
} np_callback;

static inline np_callback * np_new(value v_handler)
{
  np_callback *c;
  c = (np_callback *) caml_stat_alloc(sizeof(np_callback));
  c->v_cb = v_handler;
  c->cnt = 1;
  caml_register_generational_global_root(&(c->v_cb));
  return c;
}

static inline void np_incr_refcount(np_callback *c) { if (c) (c->cnt)++; }

static inline void np_decr_refcount(np_callback *c)
{
  if (c) {
    c->cnt--;
    if (c->cnt == 0) {
      caml_remove_generational_global_root(&c->v_cb);
      caml_stat_free(c);
    }
  }
}

/* Database Connection Functions */

/* Missing:
     PQgetssl: the SSL structure used in the connection
*/

#define get_conn(v) ((PGconn *) Field(v, 0))
#define set_conn(v, conn) (Field(v, 0) = (value) conn)

#define get_conn_cb(v) ((np_callback *) Field(v, 1))
#define set_conn_cb(v, cb) (Field(v, 1) = (value) cb)

#define get_cancel_obj(v) ((PGcancel *) Field(v, 2))
#define set_cancel_obj(v, cancel) (Field(v, 2) = (value) cancel)

CAMLprim value PQconn_isnull(value v_conn)
{
  return Val_bool((get_conn(v_conn)) ? 0 : 1);
}

static inline void free_conn(value v_conn)
{
  PGconn *conn = get_conn(v_conn);
  if (conn) {
    PGcancel *cancel = get_cancel_obj(v_conn);
    set_cancel_obj(v_conn, NULL);
    np_decr_refcount(get_conn_cb(v_conn));
    set_conn_cb(v_conn, NULL);
    set_conn(v_conn, NULL);
    caml_enter_blocking_section();
      PQfreeCancel(cancel);
      PQfinish(conn);
    caml_leave_blocking_section();
  }
}

CAMLprim value PQconnectdb_stub(value v_conn_info, value v_startonly)
{
  PGconn *conn;
  value v_conn;
  PGcancel *cancel;

  if (Bool_val(v_startonly)) {
    conn = PQconnectStart(String_val(v_conn_info));
    cancel = PQgetCancel(conn);
  }
  else {
    size_t len = caml_string_length(v_conn_info) + 1;
    char *conn_info = caml_stat_alloc(len);
    memcpy(conn_info, String_val(v_conn_info), len);
    caml_enter_blocking_section();
      conn = PQconnectdb(conn_info);
      cancel = PQgetCancel(conn);
      caml_stat_free(conn_info);
    caml_leave_blocking_section();
  }

  v_conn = caml_alloc_small(3, Abstract_tag);

  set_conn(v_conn, conn);
  set_conn_cb(v_conn, NULL);
  set_cancel_obj(v_conn, cancel);

  return v_conn;
}

CAMLprim value PQfinish_stub(value v_conn)
{
  free_conn(v_conn);
  return Val_unit;
}

CAMLprim value PQreset_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  caml_enter_blocking_section();
    PQreset(conn);
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value PQconndefaults_stub(value __unused v_unit)
{
  CAMLparam0();
  CAMLlocal2(v_res, v_el);
  PQconninfoOption *cios = PQconndefaults(), *p = cios;
  int i, j, n;

  while (p->keyword != NULL) p++;

  n = p - cios;
  p = cios;
  v_res = caml_alloc_tuple(n);

  for (i = 0; i < n; i++, p++) {
    v_el = caml_alloc_small(7, 0);
    for (j = 0; j < 7; j++) Field(v_el, j) = v_None;
    Store_field(v_res, i, v_el);
    Store_field(v_el, 0, caml_copy_string(p->keyword));
    if (p->envvar) Store_field(v_el, 1, make_some(caml_copy_string(p->envvar)));
    if (p->compiled)
      Store_field(v_el, 2, make_some(caml_copy_string(p->compiled)));
    if (p->val) Store_field(v_el, 3, make_some(caml_copy_string(p->val)));
    Store_field(v_el, 4, caml_copy_string(p->label));
    Store_field(v_el, 5, caml_copy_string(p->dispchar));
    Store_field(v_el, 6, Val_int(p->dispsize));
  };

  PQconninfoFree(cios);

  CAMLreturn(v_res);
}

static inline value make_string(const char *s)
{
  return (s ? caml_copy_string(s) : v_empty_string);
}

#define conn_info(fun, ret) \
  CAMLprim value fun##_stub(value v_conn) \
  { \
    CAMLparam1(v_conn); \
    CAMLreturn(ret(fun(get_conn(v_conn)))); \
  }

#define noalloc_conn_info(fun, ret) \
  CAMLprim value fun##_stub(value v_conn) \
  { \
    return ret(fun(get_conn(v_conn))); \
  }

#define noalloc_conn_info_intnat(fun) \
  CAMLprim intnat fun##_stub(value v_conn) \
  { \
    return fun(get_conn(v_conn)); \
  } \
  \
  CAMLprim value fun##_stub_bc(value v_conn) \
  { \
    return Val_int(fun##_stub(v_conn)); \
  }

conn_info(PQconnectPoll, Val_int)
conn_info(PQresetStart, Val_bool)
conn_info(PQresetPoll, Val_int)
conn_info(PQdb, make_string)
conn_info(PQuser, make_string)
conn_info(PQpass, make_string)
conn_info(PQhost, make_string)
conn_info(PQport, make_string)
conn_info(PQtty, make_string)
conn_info(PQoptions, make_string)
noalloc_conn_info(PQstatus, Val_int)
conn_info(PQerrorMessage, make_string)
noalloc_conn_info_intnat(PQbackendPID)
noalloc_conn_info_intnat(PQserverVersion)

/* Command Execution Functions */

struct pg_ocaml_result { PGresult *res; np_callback *cb; };

#define PG_ocaml_result_val(v) ((struct pg_ocaml_result *) Data_custom_val(v))
#define get_res(v) PG_ocaml_result_val(v)->res
#define set_res(v, result) PG_ocaml_result_val(v)->res = result

#define get_res_cb(v) PG_ocaml_result_val(v)->cb
#define set_res_cb(v, callback) PG_ocaml_result_val(v)->cb = callback

#define res_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res) \
  { \
    CAMLparam1(v_res); \
    CAMLreturn(ret(fun(get_res(v_res)))); \
  }

#define noalloc_res_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res) \
  { \
    return ret(fun(get_res(v_res))); \
  }

#define noalloc_res_info_intnat(fun) \
  CAMLprim intnat fun##_stub(value v_res) \
  { \
    return fun(get_res(v_res)); \
  } \
  \
  CAMLprim value fun##_stub_bc(value v_res) \
  { \
    return Val_int(fun##_stub(v_res)); \
  }

#define fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, intnat field_num) \
  { \
    CAMLparam1(v_res); \
    CAMLreturn(ret(fun(get_res(v_res), field_num))); \
  } \
  \
  CAMLprim value fun##_stub_bc(value v_res, value v_field_num) \
  { \
    return fun##_stub(v_res, Int_val(v_field_num)); \
  }

#define noalloc_fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, intnat field_num) \
  { \
    return ret(fun(get_res(v_res), field_num)); \
  } \
  \
  CAMLprim value fun##_stub_bc(value v_res, value v_field_num) \
  { \
    return fun##_stub(v_res, Int_val(v_field_num)); \
  }

#define noalloc_fieldnum_info_intnat(fun) \
  CAMLprim intnat fun##_stub(value v_res, intnat field_num) \
  { \
    return fun(get_res(v_res), field_num); \
  } \
  \
  CAMLprim value fun##_stub_bc(value v_res, value v_field_num) \
  { \
    return Val_int(fun##_stub(v_res, Int_val(v_field_num))); \
  }

#define field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_tup_num, value v_field_num) \
  { \
    CAMLparam1(v_res); \
    CAMLreturn( \
      ret(fun(get_res(v_res), Long_val(v_tup_num), Long_val(v_field_num)))); \
  }

#define noalloc_field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, intnat tup_num, intnat field_num) \
  { \
    return ret(fun(get_res(v_res), tup_num, field_num)); \
  } \
  \
  CAMLprim value \
  fun##_stub_bc(value v_res, value v_tup_num, value v_field_num) \
  { \
    return fun##_stub(v_res, Int_val(v_tup_num), Int_val(v_field_num)); \
  }

#define noalloc_field_info_intnat(fun) \
  CAMLprim intnat fun##_stub(value v_res, intnat tup_num, intnat field_num) \
  { \
    return fun(get_res(v_res), tup_num, field_num); \
  } \
  \
  CAMLprim value \
  fun##_stub_bc(value v_res, value v_tup_num, value v_field_num) \
  { \
    return \
      Val_int(fun##_stub(v_res, Int_val(v_tup_num), Int_val(v_field_num))); \
  }


static inline void free_result(value v_res)
{
  PGresult *res;
  np_decr_refcount(get_res_cb(v_res));
  set_res_cb(v_res, NULL);
  res = get_res(v_res);
  if (res) {
    set_res(v_res, NULL);
    PQclear(res);
  }
}

CAMLprim value PQres_isnull(value v_res)
{
  return Val_bool(get_res(v_res) ? 0 : 1);
}

static struct custom_operations result_ops = {
  "pg_ocaml_result",
  free_result,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static inline value alloc_result(PGresult *res, np_callback *cb)
{
  value v_res;
  size_t result_size, ocaml_result_size = sizeof(struct pg_ocaml_result);
#if PG_OCAML_MAJOR_VERSION < 12
  /* There isn't really a bound on size here, it all depends on the number
     of records, columns, and type of data.  4096 bytes seems like a reasonable
     size that shouldn't lead to excessive GC pressure. */
  result_size = 4096;
#else
  result_size = PQresultMemorySize(res);
#endif
  v_res = caml_alloc_custom_mem(&result_ops, ocaml_result_size, result_size);
  set_res(v_res, res);
  set_res_cb(v_res, cb);
  np_incr_refcount(cb);
  return v_res;
}

static inline void copy_binary_params(
  value v_params, value v_binary_params, size_t nparams,
  int **res_formats, int **res_lengths)
{
  size_t i, nbinary = Wosize_val(v_binary_params);
  int *lengths, *formats;
  if (nbinary == 0 || nparams == 0) {
    *res_formats = NULL;
    *res_lengths = NULL;
    return;
  }
  lengths = caml_stat_alloc(nparams * sizeof(int));
  formats = caml_stat_alloc(nparams * sizeof(int));
  for (i = 0; i < nparams; i++) {
    formats[i] = 0;
    lengths[i] = 0;
  }
  if (nbinary > nparams) nbinary = nparams;
  for (i = 0; i < nbinary; i++) {
    if (Bool_val(Field(v_binary_params, i))) {
      formats[i] = 1;
      lengths[i] = caml_string_length(Field(v_params, i));
    }
  }
  *res_formats = formats;
  *res_lengths = lengths;
}

static inline void free_binary_params(int *formats, int *lengths)
{
  if (formats != NULL) caml_stat_free(formats);
  if (lengths != NULL) caml_stat_free(lengths);
}

static inline Oid * copy_param_types(
  value v_param_types, size_t nparams, size_t nparam_types)
{
   Oid *param_types;
   size_t i;
   if (nparam_types == 0) return NULL;
   if (nparam_types > nparams) nparam_types = nparams;
   param_types = caml_stat_alloc(nparams * sizeof(Oid));
   for (i = 0; i < nparam_types; i++) {
     value v_param_type = Field(v_param_types, i);
     param_types[i] = Int_val(v_param_type);
   }
   memset(
     param_types + nparam_types, 0, (nparams - nparam_types) * sizeof(Oid));
   return param_types;
}

static inline const char * const * copy_params(value v_params, size_t nparams)
{
  char **params;
  size_t i;
  if (nparams == 0) return NULL;
  params = caml_stat_alloc(nparams * sizeof(char *));
  for (i = 0; i < nparams; i++) {
    value v_param = Field(v_params, i);
    if (v_param == *v_null_param) params[i] = NULL;
    else {
      size_t param_len = caml_string_length(v_param) + 1;
      params[i] = caml_stat_alloc(param_len);
      memcpy(params[i], String_val(v_param), param_len);
    }
  }
  return (const char * const *) params;
}

static inline void free_params(const char * const *params, size_t nparams)
{
  size_t i;
  if (nparams == 0) return;
  for (i = 0; i < nparams; i++) caml_stat_free((char *) params[i]);
  caml_stat_free((char **) params);
}

static inline const char * const * copy_params_shallow(
  value v_params, size_t nparams)
{
  const char **params;
  size_t i;
  if (nparams == 0) return NULL;
  params = caml_stat_alloc(nparams * sizeof(char *));
  for (i = 0; i < nparams; i++) {
    value v_param = Field(v_params, i);
    params[i] = (v_param == *v_null_param) ? NULL : String_val(v_param);
  }
  return (const char * const *) params;
}

static inline void free_params_shallow(
  const char * const *params, size_t nparams)
{
  if (nparams == 0) return;
  caml_stat_free((char **) params);
}

CAMLprim value PQexecParams_stub(
  value v_conn, value v_query, value v_param_types, value v_params,
  value v_binary_params, value v_binary_result)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t len = caml_string_length(v_query) + 1;
  char *query = caml_stat_alloc(len);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params(v_params, nparams);
  size_t nparam_types = Wosize_val(v_param_types);
  Oid *param_types = copy_param_types(v_param_types, nparams, nparam_types);
  int *formats, *lengths;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  memcpy(query, String_val(v_query), len);
  bool binary_result = Bool_val(v_binary_result);
  caml_enter_blocking_section();
    res =
      (nparams == 0 && !binary_result)
        ? PQexec(conn, query)
        : PQexecParams(
            conn, query, nparams, param_types,
            params, lengths, formats, binary_result);
    if (param_types != NULL) caml_stat_free(param_types);
    free_binary_params(formats, lengths);
    free_params(params, nparams);
    caml_stat_free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
}

CAMLprim value PQexecParams_stub_bc(value *argv, int __unused argn)
{
  return
    PQexecParams_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

#ifdef PG_OCAML_8_2
CAMLprim value PQprepare_stub(
  value v_conn, value v_stm_name, value v_query, value v_param_types)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t stm_name_len = caml_string_length(v_stm_name) + 1;
  size_t query_len = caml_string_length(v_query) + 1;
  char *stm_name = caml_stat_alloc(stm_name_len);
  char *query = caml_stat_alloc(query_len);
  size_t nparams = Wosize_val(v_param_types);
  Oid *param_types = copy_param_types(v_param_types, nparams, nparams);
  memcpy(stm_name, String_val(v_stm_name), stm_name_len);
  memcpy(query, String_val(v_query), query_len);
  caml_enter_blocking_section();
    res = PQprepare(conn, stm_name, query, nparams, param_types);
    if (param_types != NULL) caml_stat_free(param_types);
    caml_stat_free(stm_name);
    caml_stat_free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
#else
CAMLprim value PQprepare_stub(
  value __unused v_conn, value __unused v_stm_name, value __unused v_query)
{
  caml_failwith("Postgresql.prepare: not supported");
#endif
}

#ifdef PG_OCAML_8_2
CAMLprim value PQexecPrepared_stub(
  value v_conn, value v_stm_name, value v_params, value v_binary_params)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t len = caml_string_length(v_stm_name) + 1;
  char *stm_name = caml_stat_alloc(len);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params(v_params, nparams);
  int *formats, *lengths;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  memcpy(stm_name, String_val(v_stm_name), len);
  caml_enter_blocking_section();
    res = PQexecPrepared(conn, stm_name, nparams, params, lengths, formats, 0);
    caml_stat_free(stm_name);
    free_binary_params(formats, lengths);
    free_params(params, nparams);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
#else
CAMLprim value PQexecPrepared_stub(
  value __unused v_conn, value __unused v_stm_name, value __unused v_params,
  value __unused v_binary_params)
{
  caml_failwith("Postgresql.exec_prepared: not supported");
#endif
}

#ifdef PG_OCAML_8_2
CAMLprim value PQdescribePrepared_stub(value v_conn, value v_query)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t len = caml_string_length(v_query) + 1;
  char *query = caml_stat_alloc(len);
  memcpy(query, String_val(v_query), len);
  caml_enter_blocking_section();
    res = PQdescribePrepared(conn, query);
    caml_stat_free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
#else
CAMLprim value
PQdescribePrepared_stub(value __unused v_conn, value __unused v_query)
{
  caml_failwith("Postgresql.describe_prepared: not supported");
#endif
}

noalloc_res_info(PQresultStatus, Val_int)

CAMLprim value PQresStatus_stub(value v_status)
{
  return make_string(PQresStatus(Int_val(v_status)));
}

res_info(PQresultErrorMessage, make_string)
noalloc_res_info_intnat(PQntuples)
noalloc_res_info_intnat(PQnfields)
noalloc_res_info(PQbinaryTuples, Val_bool)
fieldnum_info(PQfname, make_string)

CAMLprim value PQresultErrorField_stub(value v_res, value v_field_name)
{
  CAMLparam1(v_res);
  int field_code = error_field_tbl[Int_val(v_field_name)];
  CAMLreturn(make_string(PQresultErrorField(get_res(v_res), field_code)));
}

#ifdef PG_OCAML_8_2
noalloc_res_info_intnat(PQnparams)
#else
CAMLprim intnat PQnparams_stub(value __unused v_res)
{
  caml_failwith("Postgresql.nparams: not supported");
}

CAMLprim value PQnparams_stub_bc(value __unused v_res)
{
  return Val_int(PQnparams_stub(v_res));
}
#endif

CAMLprim intnat PQfnumber_stub(value v_res, value v_field_name)
{
  return PQfnumber(get_res(v_res), String_val(v_field_name));
}

CAMLprim value PQfnumber_stub_bc(value v_res, value v_field_name)
{
  return Val_int(PQfnumber_stub(v_res, v_field_name));
}

noalloc_fieldnum_info(PQfformat, Val_int)
noalloc_fieldnum_info_intnat(PQftype)
noalloc_fieldnum_info_intnat(PQfmod)
noalloc_fieldnum_info_intnat(PQfsize)

#ifdef PG_OCAML_8_2
noalloc_fieldnum_info_intnat(PQparamtype)
#else
CAMLprim intnat
PQparamtype_stub(value __unused v_res, intnat __unused field_num)
{
  caml_failwith("Postgresql.paramtype: not supported");
}

CAMLprim value PQparamtype_stub_bc(value v_res, value v_field_num)
{
  return Val_int(PQparamtype_stub(v_res, Int_val(v_field_num)));
}
#endif

CAMLprim value PQgetvalue_stub(value v_res, intnat tup_num, intnat field_num)
{
  CAMLparam1(v_res);
  value v_str;
  PGresult *res = get_res(v_res);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) v_str = make_string(str);
  else {
    /* Assume binary format! */
    size_t len = PQgetlength(res, tup_num, field_num);
    v_str = len ? caml_alloc_initialized_string(len, str) : v_empty_string;
  }
  CAMLreturn(v_str);
}

CAMLprim value PQgetvalue_stub_bc(
    value v_res, value v_tup_num, value v_field_num)
{
  return PQgetvalue_stub(v_res, Int_val(v_tup_num), Int_val(v_field_num));
}

/* Unescaping - auxiliary routines */

static inline bool is_bytea_hex_protocol(const char * str)
{
  return (str[0] == '\\' && str[1] == 'x');
}

static inline int is_hex_digit(char c)
{
  return (
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

static inline void raise_invalid_hex_encoding()
{
  caml_failwith("Postgresql: invalid hex encoding");
}

static size_t bytea_hex_pairs(const char *str)
{
  size_t n_hex_pairs = 0;

  /* Length calculation and encoding verification */
  while (*str != '\0') {
    if (isspace(*str)) str++;
    else if (is_hex_digit(*str)) {
      str++;
      if (is_hex_digit(*str)) { str++; n_hex_pairs++; }
      else raise_invalid_hex_encoding();
    }
    else raise_invalid_hex_encoding();
  }

  return n_hex_pairs;
}

static value unescape_bytea(const char *str)
{
  /* Old protocol */
  size_t res_len;
  char *buf = (char *) PQunescapeBytea((unsigned char*) str, &res_len);
  if (buf == NULL) caml_failwith("Postgresql: illegal bytea string");
  else {
    value v_res = caml_alloc_initialized_string(res_len, buf);
    PQfreemem(buf);
    return v_res;
  }
}

static inline int unhexdigit(char c)
{
  if (c >= '0' && c <= '9') return (c - '0');
  else if (c >= 'a' && c <= 'f') return (c - 'a' + 10);
  else if (c >= 'A' && c <= 'F') return (c - 'A' + 10);
  else
    /* This should never happen at this point */
    caml_failwith("Postgresql: internal error in unhexdigit");
}

static void decode_bytea_hex(const char *src, char *dst, size_t dst_len)
{
  char *end = dst + dst_len;
  while (dst < end) {
    if (isspace(*src)) src++;
    else {
      *dst = (char) ((unhexdigit(*src) << 4) | unhexdigit(src[1]));
      src += 2;
      dst++;
    }
  }
}


/* */

CAMLprim value PQgetescval_stub(value v_res, intnat tup_num, intnat field_num)
{
  CAMLparam1(v_res);
  value v_str;
  PGresult *res = get_res(v_res);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) {
    if (str == NULL || strlen(str) < 2 || !is_bytea_hex_protocol(str))
      CAMLreturn(unescape_bytea(str));
    else {
      size_t n_hex_pairs;
      str += 2;
      n_hex_pairs = bytea_hex_pairs(str);
      v_str = caml_alloc_string(n_hex_pairs);
      decode_bytea_hex(str, (char *) String_val(v_str), n_hex_pairs);
    }
  } else {
    /* Assume binary format! */
    size_t len = PQgetlength(res, tup_num, field_num);
    v_str = len ? caml_alloc_initialized_string(len, str) : v_empty_string;
  }
  CAMLreturn(v_str);
}

CAMLprim value PQgetescval_stub_bc(
    value v_res, value v_tup_num, value v_field_num)
{
  return PQgetescval_stub(v_res, Int_val(v_tup_num), Int_val(v_field_num));
}

noalloc_field_info(PQgetisnull, Val_bool)
noalloc_field_info_intnat(PQgetlength)

res_info(PQcmdStatus, make_string)
res_info(PQcmdTuples, make_string)
noalloc_res_info_intnat(PQoidValue)

CAMLprim value PQmakeEmptyPGresult_stub(value v_conn, value v_status)
{
  CAMLparam1(v_conn);
  value v_res =
    alloc_result(PQmakeEmptyPGresult(get_conn(v_conn), Int_val(v_status)),
                 get_conn_cb(v_conn));
  CAMLreturn(v_res);
}


/* Asynchronous Query Processing */

CAMLprim intnat PQsetnonblocking_stub(value v_conn, value v_arg)
{
  return PQsetnonblocking(get_conn(v_conn), Bool_val(v_arg));
}

CAMLprim value PQsetnonblocking_stub_bc(value v_conn, value v_arg)
{
  return Val_int(PQsetnonblocking_stub(v_conn, v_arg));
}

noalloc_conn_info(PQisnonblocking, Val_bool)

CAMLprim intnat PQsendQueryParams_stub(
  value v_conn, value v_query, value v_param_types, value v_params,
  value v_binary_params)
{
  PGconn *conn = get_conn(v_conn);
  const char *query = String_val(v_query);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params_shallow(v_params, nparams);
  size_t nparam_types = Wosize_val(v_param_types);
  Oid *param_types = copy_param_types(v_param_types, nparams, nparam_types);
  int *lengths, *formats;
  intnat res;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  res =
    (nparams == 0)
      ? PQsendQuery(conn, query)
      : PQsendQueryParams(
          conn, query, nparams, param_types, params, lengths, formats, 0);
  if (param_types != NULL) caml_stat_free(param_types);
  free_binary_params(formats, lengths);
  free_params_shallow(params, nparams);
  return res;
}

CAMLprim value PQsendQueryParams_stub_bc(
  value v_conn, value v_query, value v_param_types, value v_params,
  value v_binary_params)
{
  return Val_int(PQsendQueryParams_stub(
                    v_conn, v_query, v_param_types, v_params, v_binary_params));
}

CAMLprim intnat PQsendPrepare_stub(
    value v_conn, value v_stm_name, value v_query, value v_param_types)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  const char *query = String_val(v_query);
  size_t nparams = Wosize_val(v_param_types);
  Oid *param_types = copy_param_types(v_param_types, nparams, nparams);
  intnat res = PQsendPrepare(conn, stm_name, query, nparams, param_types);
  if (param_types != NULL) caml_stat_free(param_types);
  return res;
}

CAMLprim value PQsendPrepare_stub_bc(
    value v_conn, value v_stm_name, value v_query, value v_param_types)
{
  return
    Val_int(PQsendPrepare_stub(v_conn, v_stm_name, v_query, v_param_types));
}

CAMLprim intnat PQsendQueryPrepared_stub(
  value v_conn, value v_stm_name, value v_params, value v_binary_params)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params_shallow(v_params, nparams);
  int *lengths, *formats;
  intnat res;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  res =
    PQsendQueryPrepared(conn, stm_name, nparams, params, lengths, formats, 0);
  free_binary_params(formats, lengths);
  free_params_shallow(params, nparams);
  return res;
}

CAMLprim value PQsendQueryPrepared_stub_bc(
    value v_conn, value v_stm_name, value v_params, value v_binary_params)
{
  return
    Val_int(
        PQsendQueryPrepared_stub(
          v_conn, v_stm_name, v_params, v_binary_params));
}

#ifdef PG_OCAML_8_2
CAMLprim intnat PQsendDescribePrepared_stub(value v_conn, value v_stm_name)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  int res;
  res = PQsendDescribePrepared(conn, stm_name);
  return res;
}
#else
CAMLprim intnat
PQsendDescribePrepared_stub(value __unused v_conn, value __unused v_stm_name)
{
  caml_failwith("Postgresql.send_describe_prepared: not supported");
}
#endif

CAMLprim value PQsendDescribePrepared_stub_bc(value v_conn, value v_stm_name)
{
  return Val_int(PQsendDescribePrepared_stub(v_conn, v_stm_name));
}

#ifdef PG_OCAML_8_2
CAMLprim intnat PQsendDescribePortal_stub(value v_conn, value v_portal_name)
{
  PGconn *conn = get_conn(v_conn);
  const char *portal_name = String_val(v_portal_name);
  return PQsendDescribePortal(conn, portal_name);
}
#else
CAMLprim intnat
PQsendDescribePortal_stub(value __unused v_conn, value __unused v_portal_name)
{
  caml_failwith("Postgresql.send_describe_portal: not supported");
}
#endif

CAMLprim value PQsendDescribePortal_stub_bc(value v_conn, value v_portal_name)
{
  return Val_int(PQsendDescribePortal_stub(v_conn, v_portal_name));
}

#ifdef PG_OCAML_9_2
noalloc_conn_info_intnat(PQsetSingleRowMode)
#else
CAMLprim intnat PQsetSingleRowMode_stub(value __unused conn)
{
  caml_failwith("Postgresql.set_single_row_mode: not supported");
}

CAMLprim value PQsetSingleRowMode_stub_bc(value conn)
{
  return Val_int(PQsetSingleRowMode_stub(conn));
}
#endif

CAMLprim value PQgetResult_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  caml_enter_blocking_section();
    res = PQgetResult(conn);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
}

noalloc_conn_info_intnat(PQconsumeInput)

noalloc_conn_info_intnat(PQflush)
noalloc_conn_info_intnat(PQsocket)

CAMLprim value PQisBusy_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  bool res;
  caml_enter_blocking_section();
    res = PQisBusy(conn);
  caml_leave_blocking_section();
  CAMLreturn(Val_bool(res));
}

CAMLprim value PQCancel_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  if (conn == NULL) CAMLreturn(v_None);
  else {
    PGcancel *cancel = get_cancel_obj(v_conn);
    char errbuf[256];
    int res;
    caml_enter_blocking_section();
      res = PQcancel(cancel, errbuf, 256);
    caml_leave_blocking_section();
    if (res == 0) CAMLreturn(make_some(caml_copy_string(errbuf)));
    else CAMLreturn(v_None);
  }
}

CAMLprim value PQescapeStringConn_stub(
  value v_conn, value v_from, intnat pos_from, intnat len)
{
  size_t to_len = len + len + 1;
  char *buf = caml_stat_alloc(to_len);
  int error;
  size_t n_written =
    PQescapeStringConn(
      get_conn(v_conn), buf, String_val(v_from) + pos_from, len, &error);
  if (error) {
    caml_stat_free(buf);
    caml_failwith("Postgresql.escape_string_conn: failed to escape string");
  } else {
    value v_res = caml_alloc_initialized_string(n_written, buf);
    caml_stat_free(buf);
    return v_res;
  }
}

CAMLprim value PQescapeStringConn_stub_bc(
  value v_conn, value v_from, value v_pos_from, value v_len)
{
  return
    PQescapeStringConn_stub(
        v_conn, v_from, Int_val(v_pos_from), Int_val(v_len));
}

CAMLprim value PQescapeByteaConn_stub(
  value v_conn, value v_from, intnat pos_from, intnat len)
{
  size_t res_len;
  char *buf =
    (char *) PQescapeByteaConn(
      get_conn(v_conn),
      (unsigned char *) String_val(v_from) + pos_from, len, &res_len);
  value v_res = caml_alloc_initialized_string(--res_len, buf);
  PQfreemem(buf);
  return v_res;
}

CAMLprim value PQescapeByteaConn_stub_bc(
  value v_conn, value v_from, value v_pos_from, value v_len)
{
  return
    PQescapeByteaConn_stub(v_conn, v_from, Int_val(v_pos_from), Int_val(v_len));
}

/* Unescaping */

CAMLprim value PQunescapeBytea_stub(value v_from)
{
  const char *from = String_val(v_from);
  size_t from_len = caml_string_length(v_from);
  if (from_len < 2 || !is_bytea_hex_protocol(from)) return unescape_bytea(from);
  else {
    /* New protocol */
    size_t res_len = bytea_hex_pairs(from + 2);
    CAMLparam1(v_from);
    value v_res = caml_alloc_string(res_len);
    /* GC may have happened, have to use String_val on v_from again */
    decode_bytea_hex(
      String_val(v_from) + 2, (char *) String_val(v_res), res_len);
    CAMLreturn(v_res);
  }
}

/* Asynchronous Notification */

CAMLprim value PQnotifies_stub(value v_conn)
{
  CAMLparam1(v_conn);
  CAMLlocal2(v_str, v_extra);
  PGnotify *notif = PQnotifies(get_conn(v_conn));

  if (notif) {
    value v_notif;
    v_str = make_string(notif->relname);
    v_extra =
#if PG_OCAML_MAJOR_VERSION >= 9
      make_string(notif->extra);
#else
    v_empty_string;
#endif
    v_notif = caml_alloc_small(3, 0);
    Field(v_notif, 0) = v_str;
    Field(v_notif, 1) = Val_int(notif->be_pid);
    Field(v_notif, 2) = v_extra;
    PQfreemem(notif);
    CAMLreturn(make_some(v_notif));
  }
  else CAMLreturn(v_None);
}

/* Functions Associated with the COPY Command */

CAMLprim intnat PQputCopyData_stub(
  value v_conn, value v_buf, intnat pos, intnat len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf) + pos, len);
  caml_enter_blocking_section();
    res = PQputCopyData(conn, buf, len);
  caml_leave_blocking_section();
  caml_stat_free(buf);
  CAMLreturn(res);
}

CAMLprim value PQputCopyData_bc(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(PQputCopyData_stub(v_conn, v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat PQputCopyEnd_stub(value v_conn, value v_msg_opt)
{
  CAMLparam2(v_conn, v_msg_opt);
  intnat res;
  PGconn *conn = get_conn(v_conn);
  char *msg = NULL;
  if (Is_block(v_msg_opt)) {
    value v_msg = Field(v_msg_opt, 0);
    size_t msg_len = caml_string_length(v_msg);
    msg = caml_stat_alloc(msg_len + 1);
    memcpy(msg, String_val(v_msg), msg_len);
    msg[msg_len] = '\0';
  }
  caml_enter_blocking_section();
    res = PQputCopyEnd(conn, msg);
  caml_leave_blocking_section();
  if (msg) caml_stat_free(msg);
  CAMLreturn(res);
}

CAMLprim value PQputCopyEnd_bc(value v_conn, value v_msg)
{
  return Val_int(PQputCopyEnd_stub(v_conn, v_msg));
}

CAMLprim value PQgetCopyData_stub(value v_conn, intnat async)
{
  CAMLparam1(v_conn);
  CAMLlocal1(v_buf);
  value v_res;
  PGconn *conn = get_conn(v_conn);
  char *buf;
  intnat res;
  caml_enter_blocking_section();
    res = PQgetCopyData(conn, &buf, async);
  caml_leave_blocking_section();
  switch (res) {
    case 0:
      CAMLreturn(Val_int(0)); /* Get_copy_wait */
    case -1:
      CAMLreturn(Val_int(1)); /* Get_copy_end */
    case -2:
      CAMLreturn(Val_int(2)); /* Get_copy_error */
    default:
      v_buf = caml_alloc_initialized_string(res, buf);
      PQfreemem(buf);
      v_res = caml_alloc_small(1, 0); /* Get_copy_data */
      Field(v_res, 0) = v_buf;
      CAMLreturn(v_res);
  }
}

CAMLprim value PQgetCopyData_bc(value v_conn, value v_async)
{
  return PQgetCopyData_stub(v_conn, Int_val(v_async));
}

CAMLprim intnat PQgetline_stub(
  value v_conn, value v_buf, intnat pos, intnat len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = caml_stat_alloc(len);
  caml_enter_blocking_section();
    res = PQgetline(conn, buf, len);
  caml_leave_blocking_section();
  memcpy(Bytes_val(v_buf) + pos, buf, len);
  caml_stat_free(buf);
  CAMLreturn(res);
}

CAMLprim value PQgetline_stub_bc(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return Val_int(PQgetline_stub(v_conn, v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat PQgetlineAsync_stub(
  value v_conn, value v_buf, intnat pos, intnat len)
{
  return PQgetlineAsync(get_conn(v_conn), (char *) Bytes_val(v_buf) + pos, len);
}

CAMLprim value PQgetlineAsync_stub_bc(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(PQgetlineAsync_stub(v_conn, v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat PQputline_stub(value v_conn, value v_buf)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  size_t len = caml_string_length(v_buf) + 1;
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf), len);
  caml_enter_blocking_section();
    res = PQputline(conn, buf);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value PQputline_stub_bc(value v_conn, value v_buf)
{
  return Val_int(PQputline_stub(v_conn, v_buf));
}

CAMLprim intnat PQputnbytes_stub(
  value v_conn, value v_buf, intnat pos, intnat len)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf) + pos, len);
  caml_enter_blocking_section();
    res = PQputnbytes(conn, buf, len);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value PQputnbytes_stub_bc(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(PQputnbytes_stub(v_conn, v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat PQendcopy_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = PQendcopy(conn);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value PQendcopy_stub_bc(value v_conn)
{
  return Val_int(PQendcopy_stub(v_conn));
}

/* libpq Control Functions */

static inline void notice_ml(void *cb, const char *msg)
{
  value v_msg;
  /* CR mmottl for mmottl: this is not reliable and can lead to deadlocks or
     other unintended behavior, because the runtime lock may already be held
     (but not usually).  A runtime feature is needed to fully support this. */
  caml_leave_blocking_section();
    v_msg = make_string(msg);
    caml_callback(((np_callback *) cb)->v_cb, v_msg);
  caml_enter_blocking_section();
}

CAMLprim value PQsetNoticeProcessor_stub(value v_conn, value v_cb)
{
  np_decr_refcount(get_conn_cb(v_conn));
  set_conn_cb(v_conn, np_new(v_cb));
  PQsetNoticeProcessor(get_conn(v_conn), &notice_ml, get_conn_cb(v_conn));
  return Val_unit;
}

static void np_quiet(void __unused *arg, const char __unused *message)
{
}

static void np_stderr(void __unused *arg, const char *message)
{
    fprintf(stderr, "%s", message);
}

CAMLprim value PQsetNoticeProcessor_num(value v_conn, value v_cb_num)
{
  np_decr_refcount(get_conn_cb(v_conn));
  set_conn_cb(v_conn, NULL);
  switch (Int_val(v_cb_num)) {
    case 0:
      PQsetNoticeProcessor(get_conn(v_conn), np_stderr, NULL);
      break;
    case 1:
      PQsetNoticeProcessor(get_conn(v_conn), np_quiet, NULL);
      break;
    default:
      break;
  }
  return Val_unit;
}

/* Large objects */

CAMLprim intnat lo_creat_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = lo_creat(conn, INV_READ | INV_WRITE);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_creat_stub_bc(value v_conn)
{
  return Val_int(lo_creat_stub(v_conn));
}

CAMLprim intnat lo_import_stub(value v_conn, value v_fname)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  size_t len = caml_string_length(v_fname) + 1;
  char *fname = caml_stat_alloc(len);
  memcpy(fname, String_val(v_fname), len);
  caml_enter_blocking_section();
    res = lo_import(conn, fname);
    caml_stat_free(fname);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_import_stub_bc(value v_conn, value v_fname)
{
  return Val_int(lo_import_stub(v_conn, v_fname));
}

CAMLprim intnat lo_export_stub(value v_conn, intnat oid, value v_fname)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  size_t len = caml_string_length(v_fname) + 1;
  char *fname = caml_stat_alloc(len);
  memcpy(fname, String_val(v_fname), len);
  caml_enter_blocking_section();
    res = lo_export(conn, oid, fname);
    caml_stat_free(fname);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_export_stub_bc(value v_conn, value v_oid, value v_fname)
{
  return Val_int(lo_export_stub(v_conn, Int_val(v_oid), v_fname));
}

CAMLprim intnat lo_open_stub(value v_conn, intnat oid)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = lo_open(conn, oid, INV_READ | INV_WRITE);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_open_stub_bc(value v_conn, value v_oid)
{
  return Val_int(lo_open_stub(v_conn, Int_val(v_oid)));
}

CAMLprim intnat lo_close_stub(value v_conn, intnat fd)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = lo_close(conn, fd);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_close_stub_bc(value v_conn, value v_fd)
{
  return Val_int(lo_close_stub(v_conn, Int_val(v_fd)));
}

CAMLprim intnat lo_tell_stub(value v_conn, intnat fd)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = lo_tell(conn, fd);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_tell_stub_bc(value v_conn, value v_fd)
{
  return Val_int(lo_tell_stub(v_conn, Int_val(v_fd)));
}

CAMLprim intnat lo_unlink_stub(value v_conn, intnat oid)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  caml_enter_blocking_section();
    res = lo_unlink(conn, oid);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_unlink_stub_bc(value v_conn, value v_oid)
{
  return Val_int(lo_unlink_stub(v_conn, Int_val(v_oid)));
}

CAMLprim intnat lo_read_stub(
    value v_conn, intnat fd, value v_buf, intnat pos, intnat len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = caml_stat_alloc(len);
  caml_enter_blocking_section();
    res = lo_read(conn, fd, buf, len);
  caml_leave_blocking_section();
  memcpy(Bytes_val(v_buf) + pos, buf, len);
  caml_stat_free(buf);
  CAMLreturn(res);
}

CAMLprim value lo_read_stub_bc(
    value v_conn, value v_fd, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(
        lo_read_stub(
          v_conn, Int_val(v_fd), v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat lo_read_ba_stub(
    value v_conn, intnat fd, value v_buf, intnat pos, intnat len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = ((char *) Caml_ba_data_val(v_buf)) + pos;
  caml_enter_blocking_section();
    res = lo_read(conn, fd, buf, len);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_read_ba_stub_bc(
    value v_conn, value v_fd, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(
        lo_read_ba_stub(
          v_conn, Int_val(v_fd), v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat lo_write_stub(
    value v_conn, intnat fd, value v_buf, intnat pos, intnat len)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf) + pos, len);
  caml_enter_blocking_section();
    res = lo_write(conn, fd, buf, len);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_write_stub_bc(
    value v_conn, value v_fd, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(
        lo_write_stub(
          v_conn, Int_val(v_fd), v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat lo_write_ba_stub(
    value v_conn, intnat fd, value v_buf, intnat pos, intnat len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  char *buf = ((char *) Caml_ba_data_val(v_buf)) + pos;
  caml_enter_blocking_section();
    res = lo_write(conn, fd, buf, len);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_write_ba_stub_bc(
    value v_conn, value v_fd, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(
        lo_write_ba_stub(
          v_conn, Int_val(v_fd), v_buf, Int_val(v_pos), Int_val(v_len)));
}

CAMLprim intnat lo_lseek_stub(
    value v_conn, intnat fd, intnat pos, value v_whence)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  intnat res;
  int whence;
  caml_enter_blocking_section();
    switch (Int_val(v_whence)) {
      case 0 : whence = SEEK_SET; break;
      case 1 : whence = SEEK_CUR; break;
      default : whence = SEEK_END; break;
    }
    res = lo_lseek(conn, fd, pos, whence);
  caml_leave_blocking_section();
  CAMLreturn(res);
}

CAMLprim value lo_lseek_stub_bc(
    value v_conn, value v_fd, value v_pos, value v_whence)
{
  return
    Val_int(lo_lseek_stub(v_conn, Int_val(v_fd), Int_val(v_pos), v_whence));
}
