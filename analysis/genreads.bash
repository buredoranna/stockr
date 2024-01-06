#!/bin/bash

rm readstocks.r

echo "generating readstocks.r"
echo "# generated by genreads.bash" > readstocks.r

for f in `ls /home/user/data/stocks/*.csv`
do
	dataframe=`echo $f | sed 's/\// /g' | awk '{print $5}' | sed 's/\.csv//g'`
	echo "$dataframe <- read.table(\"$f\", sep=\",\", header=T)" >> readstocks.r
done
