peg: Peg.hs Peg/*.hs
	ghc --make -DMAIN -o peg -O -ltinfo Peg.hs

.PHONY: clean

clean:
	rm -f *.o Peg/*.o *.hi Peg/*.hi peg; rm -rf dist

.PHONY: edit
edit:
	vim Peg.hs Peg/*.hs *.peg Makefile README.md
