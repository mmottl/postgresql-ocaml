TARGETS = discover.bc
JBUILDER_ROOT = ../..

.PHONY: all clean

all:
	@echo jbuilder: Entering directory \`$(realpath $(PWD)/$(JBUILDER_ROOT))\'
	@jbuilder build --dev $(TARGETS)

clean:
	@jbuilder clean
