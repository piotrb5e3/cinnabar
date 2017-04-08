all:
	happy -gca ParCinnabar.y
	alex -g LexCinnabar.x
	ghc --make TestCinnabar.hs -o TestCinnabar

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCinnabar.* LexCinnabar.* ParCinnabar.* LayoutCinnabar.* SkelCinnabar.* PrintCinnabar.* TestCinnabar.* AbsCinnabar.* TestCinnabar ErrM.* SharedString.* ComposOp.* cinnabar.dtd XMLCinnabar.* Makefile*
	

