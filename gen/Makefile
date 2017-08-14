all:
	(cd src; make)
	(cd test; make)

clean:
	(cd src; make clean)
	(cd test; make clean)

run: all
	./test/prog

ghci:
	ghci -isrc test/prog.hs
