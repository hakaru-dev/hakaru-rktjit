.PHONY: GmmGibbs
GmmGibbs:
	cd ../../hakaru/ && stack ghc -- -O2 "../hakrit/test/GmmGibbs/hs/Main.hs" -o "../hakrit/test/GmmGibbs/hs/runner"
	cd ./GmmGibbs && ./hs/runner && raco test runner.rkt

lda:
	cd ../../hakaru/ && stack ghc -- -O2 "../hakrit/test/LDA/hs/Main.hs" -o "../hakrit/test/LDA/hs/runner"
	./LDA/hs/runner ../../input/news_t/
	# cd ./LDA && ./hs/runner && raco test runner.rkt

ldap:
	cd ../../hakaru/ && stack ghc -- -O2 "../hakrit/test/LDA/hs/$(file).hs" -o "../hakrit/test/LDA/hs/$(file)"
	./LDA/hs/$(file) ../../input/news_t/
