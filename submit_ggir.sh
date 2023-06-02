#!/bin/bash

#directory with all the .R, bash and pbs scripts needed for running the pipeline
rootdir=...

#directory where all the part1 RData files are stored
part1files=...

#directory where all the new batches will be stored
datadir=...

### empty output directory
read -p "Overwrite previous results? (y/n) " RESP
if [ "$RESP" = "y" ]; then
  echo "removing previous output in 3 seconds"
  sleep 3
  rm -rf $datadir/*
  ## create new batches
  echo "Start stratifying data to folder..."
  bash $rootdir/createbatches.bash $part1files
else
  echo "Submit job(s) in 3 seconds"
  sleep 3
fi

dirs=($(ls -d $datadir/batch_*))
dirslength=${#dirs[@]}
echo "Part1 RData files have been stratified to "$dirslength" batch folders"
echo ""
maxNjobs=15
for (( i=0; i<${dirslength}; i++));
do
    echo "Batch: "$i
    if [[ $jobcount -lt $maxNjobs ]]; #constrain which batches are submitted (gt=greater than, lt=less than)
    then
      otd=${dirs[$i]}
      otdtemp=$otd
      onegb=1000000000
      sizedir=$(du -sb $otd | cut -f1)
      CHECK=$((sizedir/onegb))
      echo "Batch folder size (GB): "$CHECK
      if [[ $CHECK -lt 15 ]]; then # check whether folder is smaller than 1 GB
         echo "... smaller than 1GB, so run pipeline"
         ## create or empty folder Qsub_log_files for storing the logs
        if [ ! -d "$otd/Qsub_log_files" ]
        then
          mkdir -p $otd/Qsub_log_files
        else
          rm -rf $otd/Qsub_log_files/* || true
        fi
        # Replaces slashes in paths by --- to enable passing them on to R.
	      otd=$(echo $otd | sed -e 's/\//---/g')
        rd=$(echo $rootdir | sed -e 's/\//---/g')
        echo $otd
        # Submit new job
        qsub -N "run_pipeline_200k${i}" \
          -o "$otdtemp/Qsub_log_files" \
          -e "$otdtemp/Qsub_log_files" \
          -v otd=$otd,rd=$rd $rootdir/run_ggir.pbs

        sleep 10
        jobcount=$jobcount+1
      fi
    fi
done
