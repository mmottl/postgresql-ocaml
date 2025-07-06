.PHONY: all clean doc

all:
	@dune build @install
	@make -C lib compile_commands.json

clean:
	@dune clean

doc:
	@dune build @doc
