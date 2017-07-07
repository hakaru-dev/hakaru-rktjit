#!/usr/bin/zsh

export PATH=$PATH:~/.cabal/bin/

hk=$1
siz=$2
inp=`cat ./test/input/$siz`
tps=$inp[(w)1]
wps=$inp[(w)2]
echo "docUpdate range 0-"$inp[(w)3]
du=$3

echo "running with args: $tps $wps $siz $du"
runhs () {
    if [[ ! -a ./test/hs/$hk ]]; then
	echo "executable not found compiling again"
	stack ghc -- "hs/"$hk".hs" -o "test/hs/"$hk
    fi
    echo "running haskell"
    ./test/hs/$hk $tps $wps ./test/input/$siz-arg3.csv ./test/input/$siz-arg4.csv ./test/input/$siz-arg5.csv $du ./test/output/$siz.csv
}

runrkt () {
    racket ./test/$hk.rkt $tps $wps ./test/input/$siz-arg3.csv ./test/input/$siz-arg4.csv ./test/input/$siz-arg5.csv $du ./test/output/$siz.csv
}

runhs
runrkt
echo "haskell output:"
cat ./test/output/$siz.csv
