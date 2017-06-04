#!/usr/bin/zsh

export PATH=$PATH:~/.cabal/bin/

hk=$1
tps=$2
wps=$3
siz=$4
du=$5

runhs () {
    ./test/hs/$hk $tps $wps ./test/input/$siz-arg3.csv ./test/input/$siz-arg4.csv ./test/input/$siz-arg5.csv $du ./test/output/$siz.csv
}

runrkt () {
    racket ./test/$hk.rkt $tps $wps ./test/input/$siz-arg3.csv ./test/input/$siz-arg4.csv ./test/input/$siz-arg5.csv $du ./test/output/$siz.csv
}

runhs
runrkt
