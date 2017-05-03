GHC=ghc

all: interpreter

interpreter: interpreter.hs AbsCinnabar.hs Values.hs ErrM.hs PState.hs AbsCinnabar.hs Expression.hs Statement.hs Program.hs PrintCinnabar.hs SkelCinnabar.hs LexCinnabar.hs ParCinnabar.hs
	${GHC} -o $@ $<

clean:
	-rm -f interpreter *.log *.aux *.hi *.o *.dvi
