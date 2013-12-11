all: 
	ghc --make Main.hs -o music
clean:
	rm -f music *.o *.hi
