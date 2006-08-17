-include ../Makefile.conf

OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = postgresql.mli postgresql.ml postgresql_stubs.c
CFLAGS  += -O2 -Wall -pedantic -Wno-long-long -Wno-missing-prototypes
CLIBS   = pq
RESULT  = postgresql

all:

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
