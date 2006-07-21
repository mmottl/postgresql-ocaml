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

static value empty_string = 0;
static value none = Val_int(0);

static value make_some(value v)
{
  CAMLparam1(v);
  value res = caml_alloc_small(1, 0);
  Field(res, 0) = v;
  CAMLreturn(res);
}

/* Cache for exceptions */
static value *exc_Oid = NULL;  /* Exception [Oid] */
static value *exc_InternalError = NULL;  /* Exception [InternalError] */

CAMLprim value PQocaml_init(value unit)
{
  register_global_root(&empty_string);
  empty_string = caml_alloc_string(0);
  exc_Oid = caml_named_value("Postgresql.Oid");
  exc_InternalError = caml_named_value("Postgresql.InternalError");
  return Val_unit;
}


/* Conversion functions */

static int oid_tbl[] = {
  BOOLOID, BYTEAOID, CHAROID, NAMEOID, INT8OID, INT2OID, INT2VECTOROID, INT4OID,
  REGPROCOID, TEXTOID, OIDOID, TIDOID, XIDOID, CIDOID, OIDVECTOROID, POINTOID,
  LSEGOID, PATHOID, BOXOID, POLYGONOID, LINEOID, FLOAT4OID, FLOAT8OID, ABSTIMEOID,
  RELTIMEOID, TINTERVALOID, UNKNOWNOID, CIRCLEOID, CASHOID, MACADDROID, INETOID,
  CIDROID, ACLITEMOID, BPCHAROID, VARCHAROID, DATEOID, TIMEOID, TIMESTAMPOID,
  TIMESTAMPTZOID, INTERVALOID, TIMETZOID, BITOID, VARBITOID, NUMERICOID,
  REFCURSOROID, REGPROCEDUREOID, REGOPEROID, REGOPERATOROID, REGCLASSOID,
  REGTYPEOID, RECORDOID, CSTRINGOID, ANYOID, ANYARRAYOID, VOIDOID, TRIGGEROID,
  LANGUAGE_HANDLEROID, INTERNALOID, OPAQUEOID, ANYELEMENTOID
};

CAMLprim value ftype_of_oid_stub(value v_oid) {
  int oid = Int_val(v_oid);
  int *p = oid_tbl;
  int *last = oid_tbl + sizeof(oid_tbl)/sizeof(oid_tbl[0]);
  while(p != last && *p != oid) p++;
  if (p == last) caml_raise_with_arg(*exc_Oid, v_oid);
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
  int counter;  /* reference counter; number of connections (at most 1) plus
                   results attached to the callback */
  value callback; /* the callback itself, registered as a global root */
} np_callback;

static np_callback * np_new(value handler)
{
  np_callback *c;
  c = (np_callback *) stat_alloc(sizeof(np_callback));
  c->callback = handler;
  c->counter = 1;
  register_global_root(&(c->callback));
  return c;
}

static void np_incr_refcount(np_callback *c) { if (c) (c->counter)++; }

static void np_decr_refcount(np_callback *c)
{
  if (c) {
    (c->counter)--;
    if ((c->counter) == 0) {
      remove_global_root(&(c->callback));
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

CAMLprim value PQconn_isnull(value vconn)
{
  return Val_int((get_conn(vconn)) ? 0 : 1);
}

static void free_conn(value vconn)
{
  PGconn *conn;
  np_decr_refcount(get_conn_cb(vconn));
  set_conn_cb(vconn, NULL);
  conn = get_conn(vconn);
  if (conn) PQfinish(conn);
  set_conn(vconn, NULL);
}

CAMLprim value PQconnectdb_stub(value vconn_info)
{
  PGconn *conn;
  value vconn;

  int len = string_length(vconn_info) + 1;
  char *conn_info = malloc(len);
  memcpy(conn_info, String_val(vconn_info), len);

  caml_enter_blocking_section();
    conn = PQconnectdb(conn_info);
    free(conn_info);
  caml_leave_blocking_section();

  /* One may raise this 30 to 500 for instance if the program takes
     responsibility of closing connections */
  vconn = caml_alloc_final(3, free_conn, 1, 30);

  set_conn(vconn, conn);
  set_conn_cb(vconn, NULL);

  return vconn;
}

CAMLprim value PQfinish_stub(value vconn)
{
  free_conn(vconn);
  return Val_unit;
}

CAMLprim value PQreset_stub(value vconn)
{
  CAMLparam1(vconn);
  caml_enter_blocking_section();
    PQreset(get_conn(vconn));
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value PQconndefaults_stub(value unit)
{
  CAMLparam0();
  CAMLlocal1(res);
  PQconninfoOption *cios = PQconndefaults(), *p = cios;
  int i, j, n;

  while (p->keyword != NULL) p++;

  n = p - cios;
  res = caml_alloc_tuple(n);

  for (i = 0; i < n; i++, cios++) {
    value el = caml_alloc_small(7, 0);
    for (j = 0; j < 7; j++) { Field(el, j) = none; };
    Store_field(res, i, el);
    Field(el, 0) = caml_copy_string(cios->keyword);
    modify(&Field(el, 1), caml_copy_string(cios->envvar));
    if (cios->compiled) {
      value some = make_some(caml_copy_string(cios->compiled));
      modify(&Field(el, 2), some);
    };
    if (cios->val) {
      value some = make_some(caml_copy_string(cios->val));
      modify(&Field(el, 3), some);
    };
    modify(&Field(el, 4), caml_copy_string(cios->label));
    modify(&Field(el, 5), caml_copy_string(cios->dispchar));
    modify(&Field(el, 6), Val_int(cios->dispsize));
  };

  CAMLreturn(res);
}

static inline value make_string(const char *s)
{ return (s ? caml_copy_string(s) : empty_string); }

#define conn_info(fun, ret) \
  CAMLprim value fun##_stub(value vconn) \
  { \
    CAMLparam1(vconn); \
    value v_ret = ret(fun(get_conn(vconn))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_conn_info(fun, ret) \
  CAMLprim value fun##_stub(value vconn) { return ret(fun(get_conn(vconn))); }

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
  CAMLprim value fun##_stub(value vres) { \
    CAMLparam1(vres); \
    value v_ret = ret(fun(get_res(vres))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_res_info(fun, ret) \
  CAMLprim value fun##_stub(value vres) { return ret(fun(get_res(vres))); }

#define fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value vres, value field_num) \
  { \
    CAMLparam1(vres); \
    value v_ret = ret(fun(get_res(vres), Int_val(field_num))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_fieldnum_info(fun, ret) \
  CAMLprim value fun##_stub(value vres, value field_num) \
  { \
    return ret(fun(get_res(vres), Int_val(field_num))); \
  }

#define field_info(fun, ret) \
  CAMLprim value fun##_stub(value vres, value tup_num, value field_num) \
  { \
    CAMLparam1(vres); \
    value v_ret = \
      ret(fun(get_res(vres), Int_val(tup_num), Int_val(field_num))); \
    CAMLreturn(v_ret); \
  }

#define noalloc_field_info(fun, ret) \
  CAMLprim value fun##_stub(value vres, value tup_num, value field_num) \
  { \
    return ret(fun(get_res(vres), Int_val(tup_num), Int_val(field_num))); \
  }

static void free_result(value vres)
{
  PGresult *res;
  np_decr_refcount(get_res_cb(vres));
  set_res_cb(vres, NULL);
  res = get_res(vres);
  if (res) PQclear(res);
  set_res(vres, NULL);
}

CAMLprim value PQres_isnull(value vres)
{
  return Val_int((get_res(vres)) ? 0 : 1);
}

static value alloc_result(PGresult *res, np_callback *cb)
{
  value vres = caml_alloc_final(3, free_result, 1, 500);
  set_res(vres, res);
  set_res_cb(vres, cb);
  np_incr_refcount(cb);
  return vres;
}

CAMLprim value PQexec_stub(value vconn, value vquery)
{
  CAMLparam2(vconn, vquery);
  value res = alloc_result(PQexec(get_conn(vconn), String_val(vquery)),
                           get_conn_cb(vconn));
  CAMLreturn(res);
}

noalloc_res_info(PQresultStatus, Val_int)

CAMLprim value PQresStatus_stub(value status)
{
  return make_string(PQresStatus(Int_val(status)));
}

res_info(PQresultErrorMessage, make_string)
noalloc_res_info(PQntuples, Val_int)
noalloc_res_info(PQnfields, Val_int)
noalloc_res_info(PQbinaryTuples, Val_int)
fieldnum_info(PQfname, make_string)

CAMLprim value PQfnumber_stub(value vres, value field_name)
{
  return Val_int(PQfnumber(get_res(vres), String_val(field_name)));
}

noalloc_fieldnum_info(PQfformat, Val_int)
noalloc_fieldnum_info(PQftype, Val_int)
noalloc_fieldnum_info(PQfsize, Val_int)
noalloc_fieldnum_info(PQfmod, Val_int)

CAMLprim value PQgetvalue_stub(value vres, value v_tup_num, value v_field_num)
{
  CAMLparam1(vres);
  value v_str;
  PGresult *res = get_res(vres);
  int field_num = Int_val(v_field_num);
  int tup_num = Int_val(v_tup_num);
  char *str = PQgetvalue(res, tup_num, field_num);
  if (PQfformat(res, field_num) == 0) v_str = make_string(str);
  else {
    /* Assume binary format! */
    int len = PQgetlength(res, tup_num, field_num);
    v_str = len ? empty_string : caml_alloc_string(len);
    memcpy(String_val(v_str), str, len);
  }
  CAMLreturn(v_str);
}

noalloc_field_info(PQgetlength, Val_int)
noalloc_field_info(PQgetisnull, Val_int)

res_info(PQcmdStatus, make_string)
res_info(PQcmdTuples, make_string)
noalloc_res_info(PQoidValue, Val_int)

CAMLprim value PQmakeEmptyPGresult_stub(value vconn, value status)
{
  CAMLparam1(vconn);
  value v_res =
    alloc_result(PQmakeEmptyPGresult(get_conn(vconn), Int_val(status)),
                 get_conn_cb(vconn));
  CAMLreturn(v_res);
}


/* Asynchronous Query Processing */

CAMLprim value PQsetnonblocking_stub(value vconn, value arg)
{
  return Val_int(PQsetnonblocking(get_conn(vconn), Int_val(arg)));
}

noalloc_conn_info(PQisnonblocking, Val_int)

CAMLprim value PQsendQuery_stub(value vconn, value query)
{
  return Val_int(PQsendQuery(get_conn(vconn), String_val(query)));
}

CAMLprim value PQgetResult_stub(value vconn)
{
  CAMLparam1(vconn);
  value v_res = alloc_result(PQgetResult(get_conn(vconn)), get_conn_cb(vconn));
  CAMLreturn(v_res);
}

noalloc_conn_info(PQconsumeInput, Val_int)
noalloc_conn_info(PQisBusy, Val_int)
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

CAMLprim value PQescapeBytea_stub(value v_from, value v_pos_from, value v_len)
{
  size_t len;
  char *buf =
    (char *) PQescapeBytea(
      (unsigned char *) String_val(v_from) + Int_val(v_pos_from),
      Int_val(v_len), &len);
  value v_res = caml_alloc_string(len - 1);
  memcpy(String_val(v_res), buf, len);
  free(buf);
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
    free(buf);
    return v_res;
  }
}


/* Asynchronous Notification */

CAMLprim value PQnotifies_stub(value vconn)
{
  CAMLparam1(vconn);
  PGnotify *noti = PQnotifies(get_conn(vconn));

  if (noti) {
    CAMLlocal1(str);
    value couple;
    str = make_string(noti->relname);
    couple = caml_alloc_small(2, 0);
    Field(couple, 0) = str;
    Field(couple, 1) = Val_int(noti->be_pid);
    CAMLreturn(make_some(couple));
  }
  else CAMLreturn(none);
}

/* Functions Associated with the COPY Command */

CAMLprim value PQgetline_stub(value vconn, value buf, value pos, value len)
{
  return Val_int(PQgetline(get_conn(vconn), String_val(buf) + Int_val(pos),
                           Int_val(len)));
}

CAMLprim value PQgetlineAsync_stub(value vconn, value buf, value pos, value len)
{
  return Val_int(PQgetlineAsync(get_conn(vconn), String_val(buf) + Int_val(pos),
                                Int_val(len)));
}

CAMLprim value PQputline_stub(value vconn, value string)
{
  return Val_int(PQputline(get_conn(vconn), String_val(string)));
}

CAMLprim value PQputnbytes_stub(value vconn, value buf, value pos, value len)
{
  return Val_int(PQputnbytes(get_conn(vconn), String_val(buf) + Int_val(pos),
                             Int_val(len)));
}

CAMLprim value PQendcopy_stub(value vconn)
{
  return Val_int(PQendcopy(get_conn(vconn)));
}


/* libpq Control Functions */

static void notice_ml(void *cb, const char *message)
{
  callback(((np_callback *) cb)->callback, make_string(message));
}

CAMLprim value PQsetNoticeProcessor_stub(value vconn, value cb)
{
  np_decr_refcount(get_conn_cb(vconn));
  set_conn_cb(vconn, np_new(cb));
  PQsetNoticeProcessor(get_conn(vconn), &notice_ml, get_conn_cb(vconn));
  return Val_unit;
}


/* Large objects */

CAMLprim value lo_open_stub(value vconn, value oid)
{
  return Val_int(lo_open(get_conn(vconn), Int_val(oid), INV_READ | INV_WRITE));
}

CAMLprim value lo_close_stub(value vconn, value fd)
{
  return Val_int(lo_close(get_conn(vconn), Int_val(fd)));
}

CAMLprim value lo_read_stub(value vconn, value fd,
                            value buf, value pos, value len)
{
  return Val_int(lo_read(get_conn(vconn), Int_val(fd),
                         String_val(buf) + Int_val(pos), Int_val(len)));
}

CAMLprim value lo_write_stub(value vconn, value fd,
                             value buf, value pos, value len)
{
  return Val_int(lo_write(get_conn(vconn), Int_val(fd),
                          String_val(buf) + Int_val(pos), Int_val(len)));
}

CAMLprim value lo_lseek_stub(value vconn, value fd, value pos)
{
  return Val_int(lo_lseek(get_conn(vconn), Int_val(fd), Int_val(pos), 0));
}

CAMLprim value lo_creat_stub(value vconn)
{
  return Val_int(lo_creat(get_conn(vconn), INV_READ | INV_WRITE));
}

CAMLprim value lo_tell_stub(value vconn, value fd)
{
  return Val_int(lo_tell(get_conn(vconn), Int_val(fd)));
}

CAMLprim value lo_unlink_stub(value vconn, value oid)
{
  return Val_int(lo_unlink(get_conn(vconn), Int_val(oid)));
}

CAMLprim value lo_import_stub(value vconn, value fname)
{
  return Val_int(lo_import(get_conn(vconn), String_val(fname)));
}

CAMLprim value lo_export_stub(value vconn, value oid, value fname)
{
  return Val_int(lo_export(get_conn(vconn), Int_val(oid), String_val(fname)));
}
