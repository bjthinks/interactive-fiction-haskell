fta: *.hs
	ghc --make fta && touch fta

clean:
	rm -f *.hi *.o *~ fta
