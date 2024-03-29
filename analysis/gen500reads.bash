#!/bin/bash

rm readsp500.r

echo "generating readsp500.r"
echo "# generated by readsp500.r" > readsp500.r

for f in `ls /home/userg/data/stocks/sp500/*.csv`
do
	dataframe=`echo $f | sed 's/\// /g' | awk '{print $6}' | sed 's/\.csv//g'`
	echo "$dataframe <- read.table(\"$f\", sep=\",\", header=T)" >> readsp500.r
done
