TARGETS = postgresql.cma libpostgresql_stubs.a

.PHONY: all clean

all: compile_commands.json
	@dune build $(TARGETS)

clean:
	@dune clean
	@rm -f compile_commands.json

compile_commands.json: config/dune config/discover.ml postgresql_stubs.c
	@dune rules | dune-compiledb
