#!/usr/bin/zsh

num_topics=$1 #20
num_words=$2  #7022
num_w=$3      #400
num_docs=$4   #47409
name=$5       #big

echo "$num_topics $num_words $num_w $num_docs"
echo "$num_topics $num_words $num_w $num_docs" > ./test/input/$name
shuf -i 0-$((num_topics-1)) -r -n $num_w   > ./test/input/$name-arg3.csv
shuf -i 0-$((num_words-1)) -r -n $num_docs > ./test/input/$name-arg4.csv
shuf -i 0-$((num_w-1)) -r -n $num_docs     > ./test/input/$name-arg5.csv
