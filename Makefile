ifh: *.hs
	ghc --make ifh && touch ifh

clean:
	rm -f *.hi *.o *~ ifh
