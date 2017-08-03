.PHONY: all clean doc

all:
	jbuilder build @install --dev

clean:
	jbuilder clean

doc:
	jbuilder build --dev @doc
