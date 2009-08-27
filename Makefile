ifeq ($(WINDIR),)
  FIND=find
else
  FIND=xfind
endif

all: rfx

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
	$(FIND) ./ -name "out.c" -type f -exec rm -f {} \;
	rm -f rfx 
	rm -f rfx_test

stat:
	$(FIND) ./ -name "*.hs" | xargs wc -l
