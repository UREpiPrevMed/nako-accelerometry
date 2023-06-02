#!/bin/bash
# call: sh 01_create_ggir_batches.sh [/path/] [#files]
cd $1
batchsize=$2
i=100
counter=0
for file in *.gt3x
do
   if [ $counter -eq $batchsize ]
   then
      counter=0
      i=$((i+1))
   fi   
   if [ $counter -eq 0 ]
   then	   
     echo "create batch$i"
     dir=batch$i
     mkdir $dir
   fi
   mv $file ./$dir
   counter=$((counter+1))
done
#mkdir ggir_out
