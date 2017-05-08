GHC=ghc

all: interpreter

interpreter: interpreter.hs AbsCinnabar.hs ErrM.hs Builtins.hs Block.hs StateModifiers.hs StateTypes.hs AbsCinnabar.hs Expression.hs Statement.hs Program.hs PrintCinnabar.hs SkelCinnabar.hs LexCinnabar.hs ParCinnabar.hs
	${GHC} -o $@ $<

TestCinnabar: TestCinnabar.hs AbsCinnabar.hs ErrM.hs PrintCinnabar.hs SkelCinnabar.hs LexCinnabar.hs ParCinnabar.hs
	${GHC} -o $@ $<

clean:
	-rm -f TestCinnabar interpreter *.log *.aux *.hi *.o *.dvi *.hi-boot *.o-boot
