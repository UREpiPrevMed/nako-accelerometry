#!/bin/bash

# run as: bash checkprogress.sh .
N1=($(find $1 -path "*basic*" | wc -l))
N2=($(find $1 -path "*ms2.out*" | wc -l))
ratio=`echo "scale=2; ($N2/$N1)*100" | bc`

N3=($(find $1 -path "*expanded*" | wc -l))

echo ""
echo "  "$N2"/"$N1" processed and resulting in GGIR part2 output file ("$ratio"%)"
echo "  "$N3" files/folders with expanded in their pathname"

