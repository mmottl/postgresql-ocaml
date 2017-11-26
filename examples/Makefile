TARGETS = $(addsuffix .bc, async cursor dump populate prompt test_lo)

.PHONY: all clean

all:
	@jbuilder build --dev $(TARGETS)

clean:
	@jbuilder clean
