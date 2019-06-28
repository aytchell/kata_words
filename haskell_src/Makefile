all: main

main: main.hs Words.hs
	ghc -Wall main.hs Words.hs

.PHONY: clean
clean:
	rm -f *.hi *.o main
