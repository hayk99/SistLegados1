#!/bin/bash


for file in ./*;
do 
	newname=$(echo $file | sed "s/\.cbl//")
	newname=$(echo $newname | sed "s/^.\///")
	echo ../fixDisplay.sh $file $newname;
	#remove echo if you want to do something
done
