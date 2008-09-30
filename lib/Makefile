-include ../Makefile.conf

OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = postgresql.mli postgresql.ml postgresql_stubs.c
CFLAGS += -O2 -Wall -pedantic -Wno-long-long -fPIC -DPIC
CFLAGS += -DPG_OCAML_MAJOR_VERSION=$(PG_OCAML_MAJOR_VERSION)
CFLAGS += -DPG_OCAML_MINOR_VERSION=$(PG_OCAML_MINOR_VERSION)
CLIBS   = pq
RESULT  = postgresql

all:

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
