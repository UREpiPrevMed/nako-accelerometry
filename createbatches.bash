#!/bin/bash
# run as: bash createbatches.bash *

# Attention ... needs to be replaced by complete path
datadir=/.../nako_ggir_200k

# we aim for batch size near 1u00 but try to avoid having one small batch at the end.
Nfiles=$(ls $1 | wc -l)
NBatches=$(((Nfiles + 4899) / 4900))
BestBatchSize=$(((Nfiles + NBatches - 1) / NBatches))
echo " Number of input files: "$Nfiles
echo " Proposed number of batches: "$NBatches" of size "$BestBatchSize

c=0
for f in "$1"/*; do
    if ! ((c % $BestBatchSize)); then
        folder1=$datadir/batch_$(printf "%06d\n" $c)/output_ggir/meta/basic
        mkdir -p $folder1
        folder2=$datadir/batch_$(printf "%06d\n" $c)/output_ggir/results/QC
        mkdir -p $folder2
	echo $folder1
    fi
    [[ -d "$f" ]] || cp "$f" "$folder1"
    ((c++))
done
