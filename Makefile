SOURCES = $(wildcard *.hs)
EXECUTABLES = $(SOURCES:%.hs=%.exe)

all: $(EXECUTABLES)

%.exe: %.hs
	ghc -O2 -o $@ $<

clean:
	rm -f *.{hi,o,exe}

.PHONY: all clean distclean
