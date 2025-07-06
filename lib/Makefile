TARGETS = postgresql.cma libpostgresql_stubs.a

.PHONY: all clean clean-compile-commands

all: compile_commands.json
	@dune build $(TARGETS)

clean: clean-compile-commands
	@dune clean

compile_commands.json: config/dune config/discover.ml postgresql_stubs.c
	@dune rules | dune-compiledb

clean-compile-commands:
	@rm -f compile_commands.json
