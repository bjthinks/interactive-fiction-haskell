ifh: *.hs
	ghc --make BrisbinStreet && touch BrisbinStreet

clean:
	rm -f *.hi *.o *~ BrisbinStreet
