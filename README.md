# racket jit for hakaru simplified
---
## Prerequsites to be installed
llvm v4.0.0
racket v6.9
hakaru latest

## Installing sham
```
git clone git@github.com:rjnw/sham.git
cd ./sham
raco pkg install
```

## Running tests
```
git clone git@github.com:rjnw/hakaru-rktjit.git
cd ./hakaru-rktjit
```
# if no haskell and racket file in ./test directory
```
./build-test.zsh <file name without hk from hk directory>
./run-test.zsh <test-name> {small,med,big} <docUpdate>
```

`run-test.zsh` will run both haskell and racket code with same inputs.
Right now every test need to have same function signature as
naive-bayes as the sample inputs I have are for those.
There are three input argument small, med, big. These are in
`./test/input` directory with format `small-arg<n>.csv`

New input values can be created using `./create-input.zsh` which
needs a couple of arguments like num of topics, doc size etc.