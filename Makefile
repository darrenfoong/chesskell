chesskell: chesskell.hs
	ghc -Wall -Wincomplete-uni-patterns chesskell.hs

clean:
	rm chesskell *.hi *.o
