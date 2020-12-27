.DEFAULT_GOAL := all

all: $(patsubst %.hs, %, $(wildcard *.hs))

%: %.hs
	ghc -o $@ $<
	rm -f *.hi
	rm -f *.o

.PHONY: clean
clean:
	rm -f *.hi
	rm -f *.o
	find . -maxdepth 1 -type f -executable -exec rm {} +
