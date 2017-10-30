GHCFLAGS= -dynamic # on Arch Linux

all: prog

%: %.hs
	ghc $(GHCFLAGS) --make -i../src $<

%.ghci: %.hs
	ghci $(GHCFLAGS) -i../src $<

clean:
	rm -f test prog *.o *.hi
