FIND=find

all: rfx

rfx: src/rfx.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs
#	cabal configure
#	cabal build
#	cp ./dist/build/rfx/rfx ./
	ghc --make -o rfx src/rfx.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs

test: rfx rfx_test

gui: rfx_gui

rfx_test: test/rfx_test.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs
	ghc --make -o rfx_test test/rfx_test.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs

rfx_gui: gui/rfx_gui.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs
	ghc --make -o rfx_gui gui/rfx_gui.hs src/Language/Rfx/*.hs src/Language/Rfx/Compiler/*.hs

clean:
	cabal clean
	$(FIND) ./ -name '*~' -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.o" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.hi" -type f -exec rm -f {} \;
	$(FIND) ./ -name "#*#" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.html" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.exe" -type f -exec rm -f {} \;
	$(FIND) ./ -name "out.c" -type f -exec rm -f {} \;
	rm -f rfx 
	rm -f rfx_test
	rm -f rfx_gui

stat:
	$(FIND) ./ -name "*.hs" | xargs wc -l
