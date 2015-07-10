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
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#if __GNUC__ >= 3
# define inline inline __attribute__ ((always_inline))
# define __unused __attribute__ ((unused))
#else
# define __unused
# define inline
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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

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
static value *v_exc_Oid = NULL;  /* Exception [Oid] */
static value *v_null_param = NULL;

CAMLprim value PQocaml_init(value __unused v_unit)
{
  v_empty_string = caml_alloc_string(0);
  caml_register_generational_global_root(&v_empty_string);
  v_exc_Oid = caml_named_value("Postgresql.Oid");
  v_null_param = caml_named_value("Postgresql.null");
  return Val_unit;
}

static inline value unescape_bytea_9x(const char *s);
static inline value unescape_bytea(const char *s);


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

CAMLprim value ftype_of_oid_stub(value v_oid)
{
  int oid = Int_val(v_oid);
  int *p = oid_tbl;
  int *last = oid_tbl + sizeof(oid_tbl)/sizeof(oid_tbl[0]);
  while (p != last && *p != oid) p++;
  if (p == last) caml_raise_with_arg(*v_exc_Oid, v_oid);
  return Val_int(p - oid_tbl);
}

CAMLprim value oid_of_ftype_stub(value v_ftype)
{
  return Val_int(oid_tbl[Int_val(v_ftype)]);
}


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
      free(conn_info);
    caml_leave_blocking_section();
  }

  /* One may raise this 30 to 500 for instance if the program takes
     responsibility of closing connections */
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
    value v_field;
    v_el = caml_alloc_small(7, 0);
    for (j = 0; j < 7; j++) Field(v_el, j) = v_None;
    Store_field(v_res, i, v_el);
    v_field = caml_copy_string(p->keyword);
    Store_field(v_el, 0, v_field);
    if (p->envvar) {
      v_field = make_some(caml_copy_string(p->envvar));
      caml_modify(&Field(v_el, 1), v_field);
    }
    if (p->compiled) {
      v_field = make_some(caml_copy_string(p->compiled));
      caml_modify(&Field(v_el, 2), v_field);
    };
    if (p->val) {
      v_field = make_some(caml_copy_string(p->val));
      caml_modify(&Field(v_el, 3), v_field);
    };
    v_field = caml_copy_string(p->label);
    caml_modify(&Field(v_el, 4), v_field);
    v_field = caml_copy_string(p->dispchar);
    caml_modify(&Field(v_el, 5), v_field);
    caml_modify(&Field(v_el, 6), Val_int(p->dispsize));
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

conn_info(PQconnectPoll, Val_int)
conn_info(PQresetStart, Val_int)
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
noalloc_conn_info(PQbackendPID, Val_int)
noalloc_conn_info(PQserverVersion, Val_int)

/* Command Execution Functions */

#define get_res(v) ((PGresult *) Field(v, 1))
#define set_res(v, res) (Field(v, 1) = (value) res)

#define get_res_cb(v) ((np_callback *) Field(v, 2))
#define set_res_cb(v, cb) (Field(v, 2) = (value) cb)

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

#define fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_field_num) \
  { \
    CAMLparam1(v_res); \
    CAMLreturn(ret(fun(get_res(v_res), Long_val(v_field_num)))); \
  }

#define noalloc_fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_field_num) \
  { \
    return ret(fun(get_res(v_res), Long_val(v_field_num))); \
  }

#define field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_tup_num, value v_field_num) \
  { \
    CAMLparam1(v_res); \
    CAMLreturn( \
      ret(fun(get_res(v_res), Long_val(v_tup_num), Long_val(v_field_num)))); \
  }

#define noalloc_field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_tup_num, value v_field_num) \
  { \
    return \
      ret(fun(get_res(v_res), Long_val(v_tup_num), Long_val(v_field_num))); \
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

static inline value alloc_result(PGresult *res, np_callback *cb)
{
  value v_res = caml_alloc_final(3, free_result, 1, 500);
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
  if (formats != NULL) free(formats);
  if (lengths != NULL) free(lengths);
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
  free((char **) params);
}

static inline const char * const * copy_params_shallow(
  value v_params, size_t nparams)
{
  char **params;
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
  free((char **) params);
}

CAMLprim value PQexecParams_stub(
  value v_conn, value v_query, value v_params, value v_binary_params)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t len = caml_string_length(v_query) + 1;
  char *query = caml_stat_alloc(len);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params(v_params, nparams);
  int *formats, *lengths;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  memcpy(query, String_val(v_query), len);
  caml_enter_blocking_section();
    res =
      (nparams == 0)
        ? PQexec(conn, query)
        : PQexecParams(conn, query, nparams, NULL, params, lengths, formats, 0);
    free_binary_params(formats, lengths);
    free_params(params, nparams);
    free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
}

#ifdef PG_OCAML_8_2
CAMLprim value PQprepare_stub(value v_conn, value v_stm_name, value v_query)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  np_callback *np_cb = get_conn_cb(v_conn);
  PGresult *res;
  size_t stm_name_len = caml_string_length(v_stm_name) + 1;
  size_t query_len = caml_string_length(v_query) + 1;
  char *stm_name = caml_stat_alloc(stm_name_len);
  char *query = caml_stat_alloc(query_len);
  memcpy(stm_name, String_val(v_stm_name), stm_name_len);
  memcpy(query, String_val(v_query), query_len);
  caml_enter_blocking_section();
    res = PQprepare(conn, stm_name, query, 0, NULL);
    free(stm_name);
    free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
#else
CAMLprim value PQprepare_stub(
  value __unused v_conn, value __unused v_stm_name, value __unused v_query)
{
  caml_failwith("Postgresql.prepare: not supported");
  return Val_unit;
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
    free(stm_name);
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
  return Val_unit;
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
    free(query);
  caml_leave_blocking_section();
  CAMLreturn(alloc_result(res, np_cb));
#else
CAMLprim value
PQdescribePrepared_stub(value __unused v_conn, value __unused v_query)
{
  caml_failwith("Postgresql.describe_prepared: not supported");
  return Val_unit;
#endif
}

noalloc_res_info(PQresultStatus, Val_int)

CAMLprim value PQresStatus_stub(value v_status)
{
  return make_string(PQresStatus(Int_val(v_status)));
}

res_info(PQresultErrorMessage, make_string)
noalloc_res_info(PQntuples, Val_long)
noalloc_res_info(PQnfields, Val_long)
noalloc_res_info(PQbinaryTuples, Val_bool)
fieldnum_info(PQfname, make_string)

#ifdef PG_OCAML_8_2
noalloc_res_info(PQnparams, Val_long)
#else
CAMLprim value PQnparams_stub(value __unused v_res)
{
  caml_failwith("Postgresql.nparams: not supported");
  return Val_unit;
}
#endif

CAMLprim value PQfnumber_stub(value v_res, value v_field_name)
{
  return Val_long(PQfnumber(get_res(v_res), String_val(v_field_name)));
}

noalloc_fieldnum_info(PQfformat, Val_int)
noalloc_fieldnum_info(PQftype, Val_int)
noalloc_fieldnum_info(PQfsize, Val_long)
noalloc_fieldnum_info(PQfmod, Val_int)

#ifdef PG_OCAML_8_2
noalloc_fieldnum_info(PQparamtype, Val_long)
#else
CAMLprim value
PQparamtype_stub(value __unused v_res, value __unused v_field_num)
{
  caml_failwith("Postgresql.paramtype: not supported");
  return Val_unit;
}
#endif

CAMLprim value PQgetvalue_stub(value v_res, value v_tup_num, value v_field_num)
{
  CAMLparam1(v_res);
  value v_str;
  PGresult *res = get_res(v_res);
  size_t field_num = Long_val(v_field_num);
  size_t tup_num = Long_val(v_tup_num);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) v_str = make_string(str);
  else {
    /* Assume binary format! */
    size_t len = PQgetlength(res, tup_num, field_num);
    v_str = len ? caml_alloc_string(len) : v_empty_string;
    memcpy(String_val(v_str), str, len);
  }
  CAMLreturn(v_str);
}

CAMLprim value PQgetescval_stub(value v_res, value v_tup_num, value v_field_num)
{
  CAMLparam1(v_res);
  value v_str;
  PGresult *res = get_res(v_res);
  size_t field_num = Long_val(v_field_num);
  size_t tup_num = Long_val(v_tup_num);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) {
    if (str != NULL && str[0] == '\\' && str[1] == 'x')
      v_str = unescape_bytea_9x(str + 2);
    else v_str = unescape_bytea(str);
  } else {
    /* Assume binary format! */
    size_t len = PQgetlength(res, tup_num, field_num);
    v_str = len ? caml_alloc_string(len) : v_empty_string;
    memcpy(String_val(v_str), str, len);
  }
  CAMLreturn(v_str);
}

noalloc_field_info(PQgetlength, Val_long)
noalloc_field_info(PQgetisnull, Val_bool)

res_info(PQcmdStatus, make_string)
res_info(PQcmdTuples, make_string)
noalloc_res_info(PQoidValue, Val_int)

CAMLprim value PQmakeEmptyPGresult_stub(value v_conn, value v_status)
{
  CAMLparam1(v_conn);
  value v_res =
    alloc_result(PQmakeEmptyPGresult(get_conn(v_conn), Int_val(v_status)),
                 get_conn_cb(v_conn));
  CAMLreturn(v_res);
}


/* Asynchronous Query Processing */

CAMLprim value PQsetnonblocking_stub(value v_conn, value v_arg)
{
  return Val_int(PQsetnonblocking(get_conn(v_conn), Bool_val(v_arg)));
}

noalloc_conn_info(PQisnonblocking, Val_bool)

CAMLprim value PQsendQueryParams_stub(
  value v_conn, value v_query, value v_params, value v_binary_params)
{
  PGconn *conn = get_conn(v_conn);
  const char *query = String_val(v_query);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params_shallow(v_params, nparams);
  int *lengths, *formats, res;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  res =
    (nparams == 0)
      ? PQsendQuery(conn, query)
      : PQsendQueryParams(
          conn, query, nparams, NULL, params, lengths, formats, 0);
  free_binary_params(formats, lengths);
  free_params_shallow(params, nparams);
  return Val_int(res);
}

CAMLprim value PQsendPrepare_stub(value v_conn, value v_stm_name, value v_query)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  const char *query = String_val(v_query);
  int res;
  res = PQsendPrepare(conn, stm_name, query, 0, NULL);
  return Val_int(res);
}

CAMLprim value PQsendQueryPrepared_stub(
  value v_conn, value v_stm_name, value v_params, value v_binary_params)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  size_t nparams = Wosize_val(v_params);
  const char * const *params = copy_params_shallow(v_params, nparams);
  int *lengths, *formats, res;
  copy_binary_params(v_params, v_binary_params, nparams, &formats, &lengths);
  res =
    PQsendQueryPrepared(conn, stm_name, nparams, params, lengths, formats, 0);
  free_binary_params(formats, lengths);
  free_params_shallow(params, nparams);
  return Val_int(res);
}

#ifdef PG_OCAML_8_2
CAMLprim value PQsendDescribePrepared_stub(value v_conn, value v_stm_name)
{
  PGconn *conn = get_conn(v_conn);
  const char *stm_name = String_val(v_stm_name);
  int res;
  res = PQsendDescribePrepared(conn, stm_name);
  return Val_int(res);
}
#else
CAMLprim value
PQsendDescribePrepared_stub(value __unused v_conn, value __unused v_stm_name)
{
  caml_failwith("Postgresql.send_describe_prepared: not supported");
  return Val_unit;
}
#endif

#ifdef PG_OCAML_8_2
CAMLprim value PQsendDescribePortal_stub(value v_conn, value v_portal_name)
{
  PGconn *conn = get_conn(v_conn);
  const char *portal_name = String_val(v_portal_name);
  int res;
  res = PQsendDescribePortal(conn, portal_name);
  return Val_int(res);
}
#else
CAMLprim value
PQsendDescribePortal_stub(value __unused v_conn, value __unused v_portal_name)
{
  caml_failwith("Postgresql.send_describe_portal: not supported");
  return Val_unit;
}
#endif

#ifdef PG_OCAML_9_2
noalloc_conn_info(PQsetSingleRowMode, Val_int)
#else
CAMLprim value
PQsetSingleRowMode_stub(value __unused conn)
{
  caml_failwith("Postgresql.set_single_row_mode: not supported");
  return Val_unit;
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

noalloc_conn_info(PQconsumeInput, Val_int)
noalloc_conn_info(PQisBusy, Val_bool)
noalloc_conn_info(PQflush, Val_int)
noalloc_conn_info(PQsocket, Val_int)

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
    if (res == 0) CAMLreturn(v_None);
    else CAMLreturn(make_some(caml_copy_string(errbuf)));
  }
}

CAMLprim value PQescapeStringConn_stub(
  value v_conn, value v_from, value v_pos_from, value v_len)
{
  size_t len = Long_val(v_len);
  size_t to_len = len + len + 1;
  char *buf = malloc(to_len);
  int error;
  size_t n_written =
    PQescapeStringConn(
      get_conn(v_conn),
      buf, String_val(v_from) + Long_val(v_pos_from),
      len, &error);
  if (error) {
    free(buf);
    caml_failwith("Postgresql.escape_string_conn: failed to escape string");
    return Val_unit;
  } else {
    value v_res = caml_alloc_string(n_written);
    memcpy(String_val(v_res), buf, n_written);
    free(buf);
    return v_res;
  }
}

CAMLprim value PQescapeByteaConn_stub(
  value v_conn, value v_from, value v_pos_from, value v_len)
{
  size_t len;
  char *buf =
    (char *) PQescapeByteaConn(
      get_conn(v_conn),
      (unsigned char *) String_val(v_from) + Long_val(v_pos_from),
      Long_val(v_len), &len);
  value v_res = caml_alloc_string(--len);
  memcpy(String_val(v_res), buf, len);
  PQfreemem(buf);
  return v_res;
}

static inline value unescape_bytea(const char *s)
{
  size_t len;
  value v_res;
  char *buf = (char *) PQunescapeBytea((unsigned char*) s, &len);
  if (buf == NULL) {
    caml_failwith("Postgresql.unescape_bytea: illegal bytea string");
    return Val_unit;
  }
  v_res = caml_alloc_string(len);
  memcpy(String_val(v_res), buf, len);
  PQfreemem(buf);
  return v_res;
}

CAMLprim value PQunescapeBytea_stub(value v_from)
{
  return unescape_bytea(String_val(v_from));
}

static inline value raise_invalid_hex_encoding()
{
  caml_failwith("Postgresql.unescape_bytea_9x: invalid hex encoding");
  return Val_unit;
}

static inline int unhexdigit(char c)
{
  if (c >= '0' && c <= '9') return (c - '0');
  else if (c >= 'a' && c <= 'f') return (c - 'a' + 10);
  else if (c >= 'A' && c <= 'F') return (c - 'A' + 10);
  else return raise_invalid_hex_encoding();
}

static inline int is_hex_digit(char c)
{
  return (
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'));
}

static inline value unescape_bytea_9x(const char *str)
{
  value v_res;
  char *res;
  size_t n_hex_pairs = 0;
  const char *end = str;

  /* Length calculation and encoding verification */
  while (*end != '\0') {
    if (isspace(*end)) end++;
    else if (is_hex_digit(*end)) {
      end++;
      if (is_hex_digit(*end)) { end++; n_hex_pairs++; }
      else return raise_invalid_hex_encoding();
    }
    else return raise_invalid_hex_encoding();
  }

  /* Assumption: string has not changed since length calculation above! */
  v_res = caml_alloc_string(n_hex_pairs);
  res = String_val(v_res);
  while (str < end) {
    if (isspace(*str)) str++;
    else {
      *res = (char) ((unhexdigit(*str) << 4) | unhexdigit(str[1]));
      str += 2;
      res++;
    }
  }
  return v_res;
}

CAMLprim value PQunescapeBytea9x_stub(value v_from)
{
  const char *s = String_val(v_from);
  if (s != NULL && s[0] == '\\' && s[1] == 'x') return unescape_bytea_9x(s + 2);
  else {
    caml_failwith("Postgresql.unescape_bytea_9x: hex prefix not found");
    return Val_unit;
  }
}

/* Asynchronous Notification */

CAMLprim value PQnotifies_stub(value v_conn)
{
  CAMLparam1(v_conn);
  CAMLlocal1(v_str);
  PGnotify *noti = PQnotifies(get_conn(v_conn));

  if (noti) {
    value v_pair;
    v_str = make_string(noti->relname);
    v_pair = caml_alloc_small(2, 0);
    Field(v_pair, 0) = v_str;
    Field(v_pair, 1) = Val_int(noti->be_pid);
    PQfreemem(noti);
    CAMLreturn(make_some(v_pair));
  }
  else CAMLreturn(v_None);
}

/* Functions Associated with the COPY Command */

CAMLprim value PQgetline_stub(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = caml_stat_alloc(len);
  caml_enter_blocking_section();
    v_res = Val_int(PQgetline(conn, buf, len));
  caml_leave_blocking_section();
  memcpy(String_val(v_buf) + Long_val(v_pos), buf, len);
  free(buf);
  CAMLreturn(v_res);
}

CAMLprim value PQgetlineAsync_stub(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return Val_int(PQgetlineAsync(get_conn(v_conn),
                                String_val(v_buf) + Long_val(v_pos),
                                Long_val(v_len)));
}

CAMLprim value PQputline_stub(value v_conn, value v_buf)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = caml_string_length(v_buf) + 1;
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf), len);
  caml_enter_blocking_section();
    v_res = Val_int(PQputline(conn, buf));
    free(buf);
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value PQputnbytes_stub(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf) + Long_val(v_pos), len);
  caml_enter_blocking_section();
    v_res = Val_int(PQputnbytes(conn, buf, len));
    free(buf);
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value PQendcopy_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(PQendcopy(conn));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}


/* libpq Control Functions */

static inline void notice_ml(void *cb, const char *msg)
{
  value v_msg;
  /* CR mmottl for mmottl: this is not reliable and can lead to segfaults,
     because the runtime lock may already be held (but not usually).
     A runtime feature is needed to fully support this. */
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


/* Large objects */

CAMLprim value lo_open_stub(value v_conn, value v_oid)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(lo_open(conn, Int_val(v_oid), INV_READ | INV_WRITE));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_close_stub(value v_conn, value v_fd)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(lo_close(conn, Int_val(v_fd)));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_read_stub(value v_conn, value v_fd,
                            value v_buf, value v_pos, value v_len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = caml_stat_alloc(len);
  caml_enter_blocking_section();
    v_res = Val_long(lo_read(conn, Int_val(v_fd), buf, len));
  caml_leave_blocking_section();
  memcpy(String_val(v_buf) + Long_val(v_pos), buf, len);
  free(buf);
  CAMLreturn(v_res);
}

CAMLprim value lo_read_ba_stub(value v_conn, value v_fd,
                               value v_buf, value v_pos, value v_len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = ((char *) Caml_ba_data_val(v_buf)) + Long_val(v_pos);
  caml_enter_blocking_section();
    v_res = Val_long(lo_read(conn, Int_val(v_fd), buf, len));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_write_stub(value v_conn, value v_fd,
                             value v_buf, value v_pos, value v_len)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = caml_stat_alloc(len);
  memcpy(buf, String_val(v_buf) + Long_val(v_pos), len);
  caml_enter_blocking_section();
    v_res = Val_long(lo_write(conn, Int_val(v_fd), buf, len));
    free(buf);
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_write_ba_stub(value v_conn, value v_fd,
                                value v_buf, value v_pos, value v_len)
{
  CAMLparam2(v_conn, v_buf);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = Long_val(v_len);
  char *buf = ((char *) Caml_ba_data_val(v_buf)) + Long_val(v_pos);
  caml_enter_blocking_section();
    v_res = Val_long(lo_write(conn, Int_val(v_fd), buf, len));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_lseek_stub(
  value v_conn, value v_fd, value v_pos, value v_whence)
{

  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  int whence;
  caml_enter_blocking_section();
    switch (Int_val(v_whence)) {
      case 0 : whence = SEEK_SET; break;
      case 1 : whence = SEEK_CUR; break;
      default : whence = SEEK_END; break;
    }
    v_res = Val_int(lo_lseek(conn, Int_val(v_fd), Long_val(v_pos), whence));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_creat_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(lo_creat(conn, INV_READ | INV_WRITE));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_tell_stub(value v_conn, value v_fd)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(lo_tell(conn, Int_val(v_fd)));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_unlink_stub(value v_conn, value v_oid)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  caml_enter_blocking_section();
    v_res = Val_int(lo_unlink(conn, Int_val(v_oid)));
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_import_stub(value v_conn, value v_fname)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = caml_string_length(v_fname) + 1;
  char *fname = caml_stat_alloc(len);
  memcpy(fname, String_val(v_fname), len);
  caml_enter_blocking_section();
    v_res = Val_int(lo_import(conn, fname));
    free(fname);
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}

CAMLprim value lo_export_stub(value v_conn, value v_oid, value v_fname)
{
  CAMLparam1(v_conn);
  PGconn *conn = get_conn(v_conn);
  value v_res;
  size_t len = caml_string_length(v_fname) + 1;
  char *fname = caml_stat_alloc(len);
  memcpy(fname, String_val(v_fname), len);
  caml_enter_blocking_section();
    v_res = Val_int(lo_export(conn, Int_val(v_oid), fname));
    free(fname);
  caml_leave_blocking_section();
  CAMLreturn(v_res);
}
