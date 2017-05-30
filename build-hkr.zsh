#!/usr/bin/zsh

for i in `ls ./hk`
do
    ~/.cabal/bin/prettyfull ./hk/$i > "hkr/"$i"r"
    read -d '' rktsrc <<EOF
#lang racket
(require ffi/unsafe)
(require sham/jit)
(require "../private/jit.rkt")
(require "../private/jit-utils.rkt")
(require "example-vectors.rkt")

(define src (read-file "../hkr/`echo $i`r"))
(define mod-env (compile-src src))

(define main (jit-get-function 'main mod-env))
(hakaru-defines mod-env)
EOF
#    echo $rktsrc > ./test/$i.rkt
done

