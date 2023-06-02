#!/bin/bash
# run as: bash appendbatches.bash

# specify input and output folders:
batches_path=/data.teams/leitzmann/nako/vincent/nako_ggir_200k
appended_output_path=/data.teams/leitzmann/nako/vincent/nako_ggir_200k_append_results
mkdir -p $appended_output_path

cd $appended_output_path
rm *

# identify files to be appended
dirs_ps=($(find $batches_path -type f -wholename "*/results_expanded/person_summary.csv"))
dirs_ds=($(find $batches_path -type f -wholename "*/results_expanded/day_summary.csv"))
dirs_qs=($(find $batches_path -type f -wholename "*/results_expanded/qc_summary.csv"))
dirs_ts=($(find $batches_path -type f -wholename "*/results_expanded/timeseries.csv"))
echo "Files that can be appended:"
echo " "${#dirs_ps[@]}" x person_summary.csv"
echo " "${#dirs_ds[@]}" x day_summary.csv"
echo " "${#dirs_qs[@]}" x qc_summary.csv"
echo " "${#dirs_ts[@]}" x timeseries.csv"
echo ""

# Check file sizes and warn if file is empty
echo "File sizes person summaries:"
dirslength=${#dirs_ps[@]}
for (( i=0; i<${dirslength}; i++));
do
    otd=${dirs_ps[$i]}
    sizefile=$(du -k $otd | cut -f1)
    echo $sizefile" batch"$i" --- "${otd#"$batches_path"}
done

echo ""
echo "File sizes day summaries:"
dirslength=${#dirs_ds[@]}
for (( i=0; i<${dirslength}; i++));
do
    otd=${dirs_ds[$i]}
    sizefile=$(du -k $otd | cut -f1)
    echo $sizefile" batch"$i" --- "${otd#"$batches_path"}
done

echo ""
echo "File sizes quality summaries:"
dirslength=${#dirs_qs[@]}
for (( i=0; i<${dirslength}; i++));
do
    otd=${dirs_qs[$i]}
    sizefile=$(du -k $otd | cut -f1)
    echo $sizefile" batch"$i" --- "${otd#"$batches_path"}
done

echo ""
echo "File sizes time series:"
dirslength=${#dirs_ts[@]}
for (( i=0; i<${dirslength}; i++));
do
    otd=${dirs_ts[$i]}
    sizefile=$(du -k $otd | cut -f1)
    echo $sizefile" batch"$i" --- "${otd#"$batches_path"}
done


echo ""

echo "Appending files and storing result in: "$appended_output_path
# append person_summary
if [ -f "$appended_output_path/all_person_summary.csv" ]; then
   echo " all_person_summary.csv already exists"
else
   awk '(NR == 1) || (FNR > 1)' ${dirs_ps[*]} > $appended_output_path/all_person_summary.csv
   if [ -f "$appended_output_path/all_person_summary.csv" ]; then
       echo " all_person_summary.csv created"
   fi
fi

# append day summary
if [ -f "$appended_output_path/all_day_summary.csv" ]; then
    echo " all_day_summary.csv already exists"
else
    awk '(NR == 1) || (FNR > 1)' ${dirs_ds[*]} > $appended_output_path/all_day_summary.csv
    if [ -f "$appended_output_path/all_day_summary.csv" ]; then
        echo " all_day_summary.csv created"
    fi
fi

# append qc summary
if [ -f "$appended_output_path/all_qc_summary.csv" ]; then
    echo " all_qc_summary.csv already exists"
else
    awk '(NR == 1) || (FNR > 1)' ${dirs_qs[*]} > $appended_output_path/all_qc_summary.csv
    if [ -f "$appended_output_path/all_qc_summary.csv" ]; then
       echo " all_qc_summary.csv created"
    fi
fi

# append time series
if [ -f "$appended_output_path/all_timeseries.csv" ]; then
    echo " all_timeseries.csv already exists"
else
    awk '(NR == 1) || (FNR > 1)' ${dirs_ts[*]} > $appended_output_path/all_timeseries.csv
    if [ -f "$appended_output_path/all_timeseries.csv" ]; then
        echo " all_timeseries.csv created"
    fi
fi
