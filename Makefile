FIND=xfind

all: rfx test

rfx: src/rfx.hs src/Language/Rfx/*.hs
	ghc --make -o rfx -Wall src/rfx.hs src/Language/Rfx/*.hs

test: rfx_test

rfx_test: test/rfx_test.hs src/Language/Rfx/*.hs
	ghc --make -o rfx_test -Wall test/rfx_test.hs src/Language/Rfx/*.hs

clean:
	$(FIND) ./ -name '*~' -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.o" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.hi" -type f -exec rm -f {} \;
	$(FIND) ./ -name "#*#" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.html" -type f -exec rm -f {} \;
	$(FIND) ./ -name "*.exe" -type f -exec rm -f {} \;
	rm -f rfx
	rm -f rfx_test

stat:
	$(FIND) ./ -name "*.hs" -type f -exec wc -l {} \;
	du -sb ./src
	du -sb ./test
