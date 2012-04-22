peg: Peg.hs Peg/*.hs
	ghc --make -DMAIN -o peg -O -ltinfo Peg.hs

.PHONY: clean

clean:
	rm -f *.o Peg/*.o *.hi Peg/*.hi peg; rm -rf dist
