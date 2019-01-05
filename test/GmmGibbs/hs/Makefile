.PHONY: nocat 1half 2half custom
nocat:
	cd /home/rajan/work/hakaru-benchmarks/hakaru/ && stack ghc -- -O2 "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/nocat/Main.hs" -o "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/nocat/runner"

2half:
	cd /home/rajan/work/hakaru-benchmarks/hakaru/ && stack ghc -- -O2 "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/2half/Main.hs" -o "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/2half/runner"

1half:
	cd /home/rajan/work/hakaru-benchmarks/hakaru/ && stack ghc -- -O2 "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/1half/Main.hs" -o "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/1half/runner"

custom:
	cd /home/rajan/work/hakaru-benchmarks/hakaru/ && stack ghc -- -O2 "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/$(input-file)" -o "/home/rajan/work/hakaru-rktjit/test/GmmGibbs/hs/runner"
