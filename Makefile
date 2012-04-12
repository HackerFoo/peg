peg: Peg.hs
	ghc -DMAIN -o peg -O -ltinfo Peg.hs

.PHONY: clean

clean:
	rm -f *.o peg *.hi; rm -rf dist
