#!/bin/bash
#Autor: Hayk Kocharyan


for file in ./*;
do 
	newname=$(echo $file | sed "s/\.cbl//")
	newname=$(echo $newname | sed "s/^.\///")
	./fixDisplay.sh $file $newname;
	#remove echo if you want to do something
done
