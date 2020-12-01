SOURCES = $(wildcard *.hs)
EXECUTABLES = $(SOURCES:%.hs=%.exe)

all: $(EXECUTABLES)

%.exe: %.hs
	ghc -O2 -o $@ $<

clean:
	rm -f *.{hi,o}

distclean: clean
	rm -f *.exe

.PHONY: all clean distclean
