#!/bin/bash
# call: sh run_ggir_fp.sh [/path/]
cd $1
dir="$(echo "$1" | rev | cut -d/ -f2 | rev)"
i=1
for folder in batch*
do
   if [ $i -eq 1 ]
   then
     outbatch=${folder}	   
     echo "create qscript_ggir_${dir}_${outbatch}.pbs"
     less .../nako/scripts/pbs_header_ggir > .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs
     echo "cd $1" >> .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs	
     echo "Rscript --vanilla .../nako/scripts/call_GGIR_nako_20220617.R ${1}${folder} &" >> .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs
     i=$((i+1))
   elif [ $i -eq 16 ]
   then 
     echo "Rscript --vanilla .../nako/scripts/call_GGIR_nako_20220617.R ${1}${folder} &" >> .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs
     echo "wait" >> .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs	
     i=1
   else 
     echo "Rscript --vanilla .../nako/scripts/call_GGIR_nako_20220617.R ${1}${folder} &" >> .../nako/scripts/ggir_pbs/qscript_ggir_${dir}_${outbatch}.pbs
     i=$((i+1))
   fi
done
