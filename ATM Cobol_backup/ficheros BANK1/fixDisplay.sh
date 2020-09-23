#!/bin/bash
#Autor: Hayk Kocharyan

filename=fixed/$2.cbl
echo $filename
if [ $# -ne 2 ];
then
	exit 1
else
	cat $1 | sed  's/DISPLAY *( *\([0-9]*\) * \([0-9]*\) *) *\("\."*\)/DISPLAY \3 LINE \1 COL \2/g' |
	sed  's/DISPLAY *( *\([0-9]*\) * \([0-9]*\) *) *\([^\.]*\)/DISPLAY \3 LINE \1 COL \2/g' \
	| sed 's/ACCEPT *( *\([0-9]*\) * \([0-9]*\) *) *\([^ ]*\) *\([^\.\n]*\)/ACCEPT \3 LINE \1 COL \2 \4/' > $filename

fi
