SOURCE=Peg.hs Search.hs Peg/*.hs *.peg Makefile README.md

peg: Peg.hs Search.hs Peg/*.hs
	ghc --make -DMAIN -o peg -O -ltinfo Peg.hs

peg-debug: Peg.hs Search.hs Peg/*.hs
	ghc --make -DMAIN -DDEBUG -o peg-debug -O -ltinfo Peg.hs

.PHONY: clean

clean:
	rm -f *.o Peg/*.o *.hi Peg/*.hi peg peg-debug; rm -rf dist

.PHONY: vim
vim:
	vim $(SOURCE)

.PHONY: gvim
gvim:
	gvim $(SOURCE) &

.PHONY: emacs
emacs:
	emacs $(SOURCE) &
