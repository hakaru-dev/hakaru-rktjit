#!/usr/bin/zsh

export PATH=$PATH:~/.cabal/bin/
export PATH=$PATH:~/.local/bin

hk=$1
inp="hk/$hk.hk"
echo "input=$inp"
opt=""
if [[ ! -z "$2" ]] 
then
    hk=$1"bucket"
    opt="--opt"
fi

racketcode () {
    read -d '' rktsrc <<EOF
#lang racket
(require ffi/unsafe)
(require sham/jit)
(require rackunit)
(require "../private/jit.rkt")
(require "../private/utils.rkt")
(require "../private/jit-utils.rkt")
(require "example-vectors.rkt")

(define src (read-file "hkr/`echo $hk`.hkr"))
(define mod-env (parameterize ([debug-pass #f])
                  (compile-src src)))
(hakaru-defines mod-env)
(define main (jit-get-function 'main mod-env))

(define (get-argument i)
  (vector-ref (current-command-line-arguments) i))

(define topic-prior (make-c-array-prob (replicate-vector (string->number (get-argument 0)) (real->prob 1.0))))
(define word-prior (make-c-array-prob (replicate-vector (string->number (get-argument 1)) (real->prob 1.0))))
(define v (make-c-array-nat (read-vector-from-csv (get-argument 2))))
(define words (make-c-array-nat (read-vector-from-csv (get-argument 3))))
(define docs (make-c-array-nat (read-vector-from-csv (get-argument 4))))
(define docUpdate (string->number (get-argument 5)))
(define result-raw (time (main topic-prior word-prior v words docs docUpdate)))
(define result-vector (cblock->vector (get-array-prob result-raw) prob-type (size-array-prob result-raw)))
(write-vector-to-csv result-vector)
(pretty-display result-vector)
EOF
    echo $rktsrc > ./test/$hk.rkt
}

haskellcode () {
    echo "generating haskell code"
    if [[ -z "$opt" ]] then
	compile $inp -o "hs/"$hk".hs" -M Main --logfloat-prelude
    else
	summary $inp -o "hs/"$hk".hs" -M Main --logfloat-prelude 
    fi

    import_template=`cat hs/import-template`
    main_template=`cat hs/main-template`
    before=`sed -n '1,12p' "hs/$hk.hs"`
    after=`sed -n '13,9999p' "hs/$hk.hs"`
    echo "$before\n$import_template\n$after" > "hs/"$hk".hs"
    echo $main_template >> "hs/"$hk".hs"
}

compilehaskell () {
    # ghc "hs/"$hk".hs" -o "test/hs/"$hk
    cd ../hakaru/
    stack exec -- ghc -O2 "../hakaru-rktjit/hs/"$hk".hs" -o "../hakaru-rktjit/test/hs/"$hk
}

buildhk () {
    echo "generating hkr"


    prettysexpr $opt $inp > "hkr/"$hk".hkr"

    racketcode
    haskellcode
    compilehaskell
}

buildhk
