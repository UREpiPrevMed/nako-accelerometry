# Processing NAKO accelerometer data for data quality assessment

This repository presents the processing of NAKO accelerometer data demonstrated in the manuscript: "Large-scale assessment of physical activity in a population using high-resolution hip-worn accelerometry: the German National Cohort (NAKO)" which is open access under https://doi.org/10.1038/s41598-024-58461-5.

## 1. GGIR part1

- First, assemble the .gt3x files into batches of N files (recommended 40) by running the command `sh 01_create_ggir_batches.sh [/path/] [N]`.
- Second, use `sh 02_run_ggir_fp.sh [/path/]` to create for each batch a command to run GGIR part 1 by calling `Rscript --vanilla /program-path/call_GGIR_nako_20220617.R [/path/batchX]`. 16 of these commands are compiled into a PBS queuing system job script with following parameters for the high performance computing cluster: 1 CPU with 16 kernels, 13h computing time, 64385mb memory using the following PBS-header (saved in a separate file):\
#! /bin/bash \
#PBS -l nodes=1:ppn=16 \
#PBS -l walltime=13:00:00 \
#PBS -l mem=64385mb \
#PBS -q serial \
#ATHOS -o req-ramdsk \
#ATHOS -deps SELF-PROVIDED:GGIR
- Third, create a subfolder `ggir_out` in the folder [/path/] where all the batches are located and in which the part 1 results will be stored.

The GGIR part 1 routine produces for each .gt3x file one .RData file stored in the folder `meta/basic` of the respective `/ggir_out/output_batchXX`. 

## 2. GGIR part 2

### 2.1 Create and process batches

By running the command `bash submit_ggir.sh`, the following happens:

- The GGIR part 1 output (RData) is copied from folder `RData_files_GGIRpart1_200k` to a newly created batch folders inside output folder `nako_ggir_200k`. These folder names are specified at the top of the `submit_ggir.sh`.
- The batch size is automatically calculated such that each batch has approximately the same number of files and is close to 1500 files per batch, because the pbs settings have been optimised for 1500 files per job.
- Once batches are created, and the output folder is less than 6GB the job corresponding to each batch can be submitted. However, the code submits only a maximum of `maxNjobs` jobs. The value of `maxNjobs` is set in line 16 of `submit_ggir.R`. Initially we may want to set this at 5 or less, to limit the usage of the cluster, but eventually when we feel confident that the pipeline runs robustly we can increase it. Each job will be send to one of the 16 core nodes. So, when `maxNjobs=6`, 84 files are being processed in parallel (16 - 2) x 6. The best way to use the script is to run it, wait for the jobs to be finished, check progress, and run the script again such that it starts the next 5 jobs.
- Results per batch will be stored inside each batch folder.
- The R code used for doing the actual data processing can be found in `pipeline.R`.

**Note:**
This pipeline automatically deletes and overwrites previously derived output.
If you want to compare runs of the pipeline then make your own copy for reference.

#### 2.1.1 Check progress:

Given that the processing can take up hours or even a few days it is useful to track progress. In addition to using `qstat` to see whether the pipeline is still running we included a function to print the number of GGIR part1 and part2 milestone files.

CD to the output folder that holds all the batches:

Run this command to print to the console a progress summary.

`bash /path/to/scripts/checkprogress.sh .`

### 2.2 Append the results from all batches

This step appends the results from all batches. Inside the file you can specify the path where the appended results should be stored, but default is `.../nako_ggir_200k_append_results`.

`bash appendbatches.bash`

When running the command, the code deletes previously appended output and prints to the console the file size for each file that is being appended to help detect abnormalities in the size of output files produced per batch.
