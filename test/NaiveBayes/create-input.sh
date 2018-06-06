#!/usr/bin/bash

num_topics=$1 #20
num_words=$2  #7022
num_docs=$3   #47409
max_words_per_doc=$4
name=$5

echo "$num_topics $num_words $num_docs"
# echo "$num_topics $num_words $num_docs" > ./test/input/$name
mkdir -p ./test/input/news_$name/
# rm ./test/input/news_$name/docs
# rm ./test/input/news_$name/words

shuf -i 0-$((num_topics-1)) -r -n $num_docs   > ./test/input/news_$name/topics

# shuf -i 0-$((num_words-1)) -r -n $((num_docs*max_words_per_doc)) > ./test/input/news_$name/words
# shuf -i 0-$((num_docs-1)) -r -n $((num_docs*max_words_per_doc)) > ./test/input/news_$name/docs

for i in `seq 0 $((num_docs - 1))`; do
    w=`echo $((RANDOM % max_words_per_doc))`
    for j in `seq 0 $w`; do
	echo $i >> ./test/input/news_$name/docs
	echo $((RANDOM % num_words)) >> ./test/input/news_$name/words
    done
done
