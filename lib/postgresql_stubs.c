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

/* $Id: postgresql_stubs.c,v 1.15 2006/01/27 22:59:34 mottl Exp $ */

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>

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

static value v_empty_string = 0;
static value v_None = Val_int(0);

static inline value make_some(value v)
{
  CAMLparam1(v);
  value v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = v;
  CAMLreturn(v_res);
}

/* Cache for exceptions */
static value *v_exc_Oid = NULL;  /* Exception [Oid] */

CAMLprim value PQocaml_init(value v_unit)
{
  register_global_root(&v_empty_string);
  v_empty_string = caml_alloc_string(0);
  v_exc_Oid = caml_named_value("Postgresql.Oid");
  return Val_unit;
}


/* Conversion functions */

static int oid_tbl[] = {
  BOOLOID, BYTEAOID, CHAROID, NAMEOID, INT8OID, INT2OID, INT2VECTOROID,
  INT4OID, REGPROCOID, TEXTOID, OIDOID, TIDOID, XIDOID, CIDOID,
  OIDVECTOROID, POINTOID, LSEGOID, PATHOID, BOXOID, POLYGONOID, LINEOID,
  FLOAT4OID, FLOAT8OID, ABSTIMEOID, RELTIMEOID, TINTERVALOID, UNKNOWNOID,
  CIRCLEOID, CASHOID, MACADDROID, INETOID, CIDROID, ACLITEMOID,
  BPCHAROID, VARCHAROID, DATEOID, TIMEOID, TIMESTAMPOID, TIMESTAMPTZOID,
  INTERVALOID, TIMETZOID, BITOID, VARBITOID, NUMERICOID, REFCURSOROID,
  REGPROCEDUREOID, REGOPEROID, REGOPERATOROID, REGCLASSOID, REGTYPEOID,
  RECORDOID, CSTRINGOID, ANYOID, ANYARRAYOID, VOIDOID, TRIGGEROID,
  LANGUAGE_HANDLEROID, INTERNALOID, OPAQUEOID, ANYELEMENTOID
};

CAMLprim value ftype_of_oid_stub(value v_oid) {
  int oid = Int_val(v_oid);
  int *p = oid_tbl;
  int *last = oid_tbl + sizeof(oid_tbl)/sizeof(oid_tbl[0]);
  while (p != last && *p != oid) p++;
  if (p == last) caml_raise_with_arg(*v_exc_Oid, v_oid);
  return Val_int (p - oid_tbl);
}

CAMLprim value oid_of_ftype_stub(value v_ftype) {
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
  register_global_root(&(c->v_cb));
  return c;
}

static inline void np_incr_refcount(np_callback *c) { if (c) (c->cnt)++; }

static inline void np_decr_refcount(np_callback *c)
{
  if (c) {
    c->cnt--;
    if (c->cnt == 0) {
      remove_global_root(&c->v_cb);
      stat_free(c);
    }
  }
}

/* Database Connection Functions */

/* Missing:
     PQconnectStart, PQconnectPoll, PQresetStart, PQresetPoll:
       for non-blocking connection
     PQgetssl: the SSL structure used in the connection
*/

#define get_conn(v) ((PGconn *) Field(v, 1))
#define set_conn(v, conn) (Field(v, 1) = (value) conn)

#define get_conn_cb(v) ((np_callback *) Field(v, 2))
#define set_conn_cb(v, cb) (Field(v, 2) = (value) cb)

CAMLprim value PQconn_isnull(value v_conn)
{
  return Val_bool((get_conn(v_conn)) ? 0 : 1);
}

static inline void free_conn(value v_conn)
{
  PGconn *conn;
  np_decr_refcount(get_conn_cb(v_conn));
  set_conn_cb(v_conn, NULL);
  conn = get_conn(v_conn);
  if (conn) PQfinish(conn);
  set_conn(v_conn, NULL);
}

CAMLprim value PQconnectdb_stub(value v_conn_info)
{
  PGconn *conn;
  value v_conn;

  int len = string_length(v_conn_info) + 1;
  char *conn_info = malloc(len);
  memcpy(conn_info, String_val(v_conn_info), len);

  caml_enter_blocking_section();
    conn = PQconnectdb(conn_info);
    free(conn_info);
  caml_leave_blocking_section();

  /* One may raise this 30 to 500 for instance if the program takes
     responsibility of closing connections */
  v_conn = caml_alloc_final(3, free_conn, 1, 30);

  set_conn(v_conn, conn);
  set_conn_cb(v_conn, NULL);

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
  caml_enter_blocking_section();
    PQreset(get_conn(v_conn));
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value PQconndefaults_stub(value v_unit)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  PQconninfoOption *cios = PQconndefaults(), *p = cios;
  int i, j, n;

  while (p->keyword != NULL) p++;

  n = p - cios;
  v_res = caml_alloc_tuple(n);

  for (i = 0; i < n; i++, cios++) {
    value v_el = caml_alloc_small(7, 0);
    for (j = 0; j < 7; j++) { Field(v_el, j) = v_None; };
    Store_field(v_res, i, v_el);
    Field(v_el, 0) = caml_copy_string(cios->keyword);
    modify(&Field(v_el, 1), caml_copy_string(cios->envvar));
    if (cios->compiled) {
      value v_Some = make_some(caml_copy_string(cios->compiled));
      modify(&Field(v_el, 2), v_Some);
    };
    if (cios->val) {
      value v_Some = make_some(caml_copy_string(cios->val));
      modify(&Field(v_el, 3), v_Some);
    };
    modify(&Field(v_el, 4), caml_copy_string(cios->label));
    modify(&Field(v_el, 5), caml_copy_string(cios->dispchar));
    modify(&Field(v_el, 6), Val_int(cios->dispsize));
  };

  CAMLreturn(v_res);
}

static inline value make_string(const char *s)
{ return (s ? caml_copy_string(s) : v_empty_string); }

#define conn_info(fun, ret) \
  CAMLprim value fun##_stub(value v_conn) \
  { \
    CAMLparam1(v_conn); \
    value v_ret = ret(fun(get_conn(v_conn))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_conn_info(fun, ret) \
  CAMLprim value fun##_stub(value v_conn) { return ret(fun(get_conn(v_conn))); }

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


/* Command Execution Functions */

#define get_res(v) ((PGresult *) Field(v, 1))
#define set_res(v, res) (Field(v, 1) = (value) res)

#define get_res_cb(v) ((np_callback *) Field(v, 2))
#define set_res_cb(v, cb) (Field(v, 2) = (value) cb)

#define res_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res) { \
    CAMLparam1(v_res); \
    value v_ret = ret(fun(get_res(v_res))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_res_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res) { return ret(fun(get_res(v_res))); }

#define fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_field_num) \
  { \
    CAMLparam1(v_res); \
    value v_ret = ret(fun(get_res(v_res), Int_val(v_field_num))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_field_num) \
  { \
    return ret(fun(get_res(v_res), Int_val(v_field_num))); \
  }

#define field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_tup_num, value v_field_num) \
  { \
    CAMLparam1(v_res); \
    value v_ret = \
      ret(fun(get_res(v_res), Int_val(v_tup_num), Int_val(v_field_num))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_field_info(fun, ret) \
  CAMLprim value fun##_stub(value v_res, value v_tup_num, value v_field_num) \
  { \
    return ret(fun(get_res(v_res), Int_val(v_tup_num), Int_val(v_field_num))); \
  }

static inline void free_result(value v_res)
{
  PGresult *res;
  np_decr_refcount(get_res_cb(v_res));
  set_res_cb(v_res, NULL);
  res = get_res(v_res);
  if (res) PQclear(res);
  set_res(v_res, NULL);
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

CAMLprim value PQexec_stub(value v_conn, value v_query)
{
  CAMLparam2(v_conn, v_query);
  value v_res = alloc_result(PQexec(get_conn(v_conn), String_val(v_query)),
                             get_conn_cb(v_conn));
  CAMLreturn(v_res);
}

noalloc_res_info(PQresultStatus, Val_int)

CAMLprim value PQresStatus_stub(value v_status)
{
  return make_string(PQresStatus(Int_val(v_status)));
}

res_info(PQresultErrorMessage, make_string)
noalloc_res_info(PQntuples, Val_int)
noalloc_res_info(PQnfields, Val_int)
noalloc_res_info(PQbinaryTuples, Val_bool)
fieldnum_info(PQfname, make_string)

CAMLprim value PQfnumber_stub(value v_res, value v_field_name)
{
  return Val_int(PQfnumber(get_res(v_res), String_val(v_field_name)));
}

noalloc_fieldnum_info(PQfformat, Val_int)
noalloc_fieldnum_info(PQftype, Val_int)
noalloc_fieldnum_info(PQfsize, Val_int)
noalloc_fieldnum_info(PQfmod, Val_int)

CAMLprim value PQgetvalue_stub(value v_res, value v_tup_num, value v_field_num)
{
  CAMLparam1(v_res);
  value v_str;
  PGresult *res = get_res(v_res);
  int field_num = Int_val(v_field_num);
  int tup_num = Int_val(v_tup_num);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) v_str = make_string(str);
  else {
    /* Assume binary format! */
    int len = PQgetlength(res, tup_num, field_num);
    v_str = len ? v_empty_string : caml_alloc_string(len);
    memcpy(String_val(v_str), str, len);
  }
  CAMLreturn(v_str);
}

noalloc_field_info(PQgetlength, Val_int)
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

CAMLprim value PQsendQuery_stub(value v_conn, value v_query)
{
  return Val_int(PQsendQuery(get_conn(v_conn), String_val(v_query)));
}

CAMLprim value PQgetResult_stub(value v_conn)
{
  CAMLparam1(v_conn);
  value v_res =
    alloc_result(PQgetResult(get_conn(v_conn)), get_conn_cb(v_conn));
  CAMLreturn(v_res);
}

noalloc_conn_info(PQconsumeInput, Val_int)
noalloc_conn_info(PQisBusy, Val_bool)
noalloc_conn_info(PQflush, Val_int)
noalloc_conn_info(PQsocket, Val_int)
noalloc_conn_info(PQrequestCancel, Val_int)

CAMLprim value PQescapeString_stub(value v_from, value v_pos_from,
                                   value v_to, value v_pos_to, value v_len)
{
  return Val_int(PQescapeString(String_val(v_to) + Int_val(v_pos_to),
                                String_val(v_from) + Int_val(v_pos_from),
                                Int_val(v_len)));
}

CAMLprim value PQescapeByteaConn_stub(
  value v_conn, value v_from, value v_pos_from, value v_len)
{
  size_t len;
  char *buf =
    (char *) PQescapeByteaConn(
      get_conn(v_conn),
      (unsigned char *) String_val(v_from) + Int_val(v_pos_from),
      Int_val(v_len), &len);
  value v_res = caml_alloc_string(--len);
  memcpy(String_val(v_res), buf, len);
  PQfreemem(buf);
  return v_res;
}

CAMLprim value PQunescapeBytea_stub(value v_from)
{
  size_t len;
  char *buf =
    (char *) PQunescapeBytea((unsigned char *) String_val(v_from), &len);
  if (buf == NULL)
    failwith("Postgresql.unescape_bytea: illegal bytea string");
  else {
    value v_res = caml_alloc_string(len);
    memcpy(String_val(v_res), buf, len);
    PQfreemem(buf);
    return v_res;
  }
}


/* Asynchronous Notification */

CAMLprim value PQnotifies_stub(value v_conn)
{
  CAMLparam1(v_conn);
  PGnotify *noti = PQnotifies(get_conn(v_conn));

  if (noti) {
    CAMLlocal1(str);
    value v_pair;
    str = make_string(noti->relname);
    v_pair = caml_alloc_small(2, 0);
    Field(v_pair, 0) = str;
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
  return Val_int(PQgetline(get_conn(v_conn), String_val(v_buf) + Int_val(v_pos),
                           Int_val(v_len)));
}

CAMLprim value PQgetlineAsync_stub(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return Val_int(PQgetlineAsync(get_conn(v_conn),
                                String_val(v_buf) + Int_val(v_pos),
                                Int_val(v_len)));
}

CAMLprim value PQputline_stub(value v_conn, value v_str)
{
  return Val_int(PQputline(get_conn(v_conn), String_val(v_str)));
}

CAMLprim value PQputnbytes_stub(
  value v_conn, value v_buf, value v_pos, value v_len)
{
  return
    Val_int(PQputnbytes(get_conn(v_conn), String_val(v_buf) + Int_val(v_pos),
                        Int_val(v_len)));
}

CAMLprim value PQendcopy_stub(value v_conn)
{
  return Val_int(PQendcopy(get_conn(v_conn)));
}


/* libpq Control Functions */

static inline void notice_ml(void *cb, const char *message)
{
  value v_message = make_string(message);
  caml_callback(((np_callback *) cb)->v_cb, v_message);
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
  return
    Val_int(lo_open(get_conn(v_conn), Int_val(v_oid), INV_READ | INV_WRITE));
}

CAMLprim value lo_close_stub(value v_conn, value v_fd)
{
  return Val_int(lo_close(get_conn(v_conn), Int_val(v_fd)));
}

CAMLprim value lo_read_stub(value v_conn, value v_fd,
                            value v_buf, value v_pos, value v_len)
{
  return Val_int(lo_read(get_conn(v_conn), Int_val(v_fd),
                         String_val(v_buf) + Int_val(v_pos), Int_val(v_len)));
}

CAMLprim value lo_write_stub(value v_conn, value v_fd,
                             value v_buf, value v_pos, value v_len)
{
  return Val_int(lo_write(get_conn(v_conn), Int_val(v_fd),
                          String_val(v_buf) + Int_val(v_pos), Int_val(v_len)));
}

CAMLprim value lo_lseek_stub(value v_conn, value v_fd, value v_pos)
{
  return Val_int(lo_lseek(get_conn(v_conn), Int_val(v_fd), Int_val(v_pos), 0));
}

CAMLprim value lo_creat_stub(value v_conn)
{
  return Val_int(lo_creat(get_conn(v_conn), INV_READ | INV_WRITE));
}

CAMLprim value lo_tell_stub(value v_conn, value v_fd)
{
  return Val_int(lo_tell(get_conn(v_conn), Int_val(v_fd)));
}

CAMLprim value lo_unlink_stub(value v_conn, value v_oid)
{
  return Val_int(lo_unlink(get_conn(v_conn), Int_val(v_oid)));
}

CAMLprim value lo_import_stub(value v_conn, value v_fname)
{
  return Val_int(lo_import(get_conn(v_conn), String_val(v_fname)));
}

CAMLprim value lo_export_stub(value v_conn, value v_oid, value v_fname)
{
  return
    Val_int(lo_export(get_conn(v_conn), Int_val(v_oid), String_val(v_fname)));
}
