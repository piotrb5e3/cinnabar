GHC=ghc

all: interpreter

interpreter: interpreter.hs AbsCinnabar.hs ErrM.hs Builtins.hs Block.hs StateModifiers.hs StateTypes.hs AbsCinnabar.hs Expression.hs Statement.hs Program.hs PrintCinnabar.hs SkelCinnabar.hs LexCinnabar.hs ParCinnabar.hs
	${GHC} -o $@ $<

TestCinnabar: TestCinnabar.hs AbsCinnabar.hs ErrM.hs PrintCinnabar.hs SkelCinnabar.hs LexCinnabar.hs ParCinnabar.hs
	${GHC} -o $@ $<

parsers:
	bnfc --haskell cinnabar.cf
	happy -gca ParCinnabar.y
	alex -g LexCinnabar.x

doc: cinnabar.pdf
cinnabar.pdf: cinnabar.tex
	pdflatex -shell-escape -interaction=nonstopmode -file-line-error cinnabar.tex

clean:
	-rm -rf TestCinnabar interpreter _minted* *.log *.aux *.hi *.o *.dvi *.hi-boot *.o-boot *.pdf
