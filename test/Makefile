.PHONY: GmmGibbs
GmmGibbs:
	cd ../../hakaru/ && stack ghc -- -O2 "../hakrit/test/GmmGibbs/hs/Main.hs" -o "../hakrit/test/GmmGibbs/hs/runner"
	cd ./GmmGibbs && ./hs/runner && raco test runner.rkt
