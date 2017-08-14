all: prog

%: %.hs
	ghc $(GHCFLAGS) --make -i../src $<

clean:
	rm -f test prog *.o *.hi
