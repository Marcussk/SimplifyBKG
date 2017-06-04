#
# simplify-bkg makefile
# Author: Marek Beňo
# xbenom01
#
LOGIN=xbenom01
NAME=simplify-bkg
ARCHIVENAME=flp-fun-$(LOGIN)
FILES=Makefile Main.hs GrammarParser.hs Types.hs README.md test.in ArgumentParser.hs

all:
	ghc --make Main.hs -o $(NAME)

run1: all
	./$(NAME) -i test.in

run2: all
	./$(NAME) -1 test.in

run3: all
	./$(NAME) -2 test.in

doc: all
	haddock Main.hs -h -o doc

archive: all
	zip $(ARCHIVENAME).zip $(FILES)

clean:
	rm -f simplify-bkg
	rm -f *.hi
	rm -f *.o
	rm -rf doc/
