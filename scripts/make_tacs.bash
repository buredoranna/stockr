#!/bin/bash

usendays=3000

for f in `ls *.csv`
do
	nlines=`wc -l $f | awk '{print $1}'`
	if [ $nlines -gt $usendays ]
	then
		echo $f
		outname=`echo -n $f | sed 's/\.csv//g'`
		echo "Date,Open,High,Low,Close,Volume,AdjClose" > tacs/$outname
		head -3001 $f | grep -vi date | tac | grep -vi date >> tacs/$outname
	fi
done
