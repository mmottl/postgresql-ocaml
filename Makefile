.PHONY: all clean doc

all:
	@dune build @install
	@make -C lib compile_commands.json

clean:
	@dune clean
	@make -C lib clean-compile-commands

doc:
	@dune build @doc
