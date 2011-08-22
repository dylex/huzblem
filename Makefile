default: huzblem
%: %.hs FORCE
	ghc --make -threaded -O -Wall $< -i -i. -o $@
clean:
	rm -f *.o *.hi huzblem
.PHONY: FORCE clean
