#!/usr/bin/env/Rscript
# R script to run for the NAKO study, by V. van Hees, A. Weber, H.Baurecht
# 26.5.2020
# GGIR - with gunzip implemented
# Andrea experiment accelerometry
#----------------------------------------------
# Packages:
# list.of.packages <- c("devtools","Rcpp","R.utils")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Packages"])]
# if(length(new.packages)){install.packages(new.packages}
# this works:
#remotes::install_github("wadpac/GGIR", ref = "issue289_unzipping", force = F)
# after Vincent moved code to branch NAKO with minimum R version set to 3.2:
#remotes::install_github("wadpac/GGIR", ref = "NAKO", force = F)
#----------------------------------------------
options(echo=TRUE)
path=commandArgs(TRUE)[1]

library(devtools)
library(Rcpp)
library(R.utils)
library(GGIR)

datadir = path
tmp<-unlist(strsplit(path,split="/",fixed=T))
outputdir = paste(paste(tmp[-length(tmp)],collapse="/"),"ggir_out",sep="/") 

#f0 = c(1)
#f1 = c(6)

time0 = Sys.time()
#=====================================================================================
# load functions directly from local clone of the R package repository
#dirR = "~/GGIR/R"
#ffnames = dir(dirR) # creating list of filenames of scriptfiles to load
#for (i in 1:length(ffnames)) {
#  source(paste(dirR,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
#}

g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode=c(1), #specify above
             datadir=datadir, #specify above
             outputdir=outputdir, #specify above
             do.report=c(), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             #f0=f0, #specify above
             #f1=f1, #specify above
             overwrite = T, #overwrite previous milestone data?
             do.parallel = F,
             idloc=1, #id location (1 = file header, 2 = filename)
             print.filename=TRUE,
             storefolderstructure = TRUE,
             # data_cleaning_file = data_cleaning_file,
             desiredtz = "Europe/Berlin",
	     do.call = FALSE, # turn auto-calibration off (which is NOT recommended) 
             #-------------------------------
             # Part 1 parameters:
             #-------------------------------
             # Key functions: reading file, auto-calibration, and extracting features
             windowsizes = c(5,900,3600), #Epoch length, non-wear detection resolution, non-wear detection evaluation window
             do.enmo = TRUE, #Needed for physical activity analysis
             do.mad = TRUE,
             do.bfen = FALSE, #TRUE,
             do.bfx= FALSE, #TRUE,
             do.bfy= FALSE, #TRUE,
             do.bfz= FALSE, #TRUE,
             do.anglex=TRUE,
             do.angley=TRUE,
             do.anglez=TRUE,
             lb = 0.3,
             hb = 20,
             n = 4,
             chunksize=0.5, #size of data chunks to be read (value = 1 is maximum)
             printsummary=TRUE,
             #-------------------------------
             # Part 2 parameters:
             #-------------------------------
             # Key functions: Non-wear detection, imputation, and basic descriptives
             strategy =4, #Strategy (see tutorial for explanation)
             ndayswindow=7, #only relevant when strategy = 3
             hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
             hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
             maxdur = 8, # How many DAYS of measurement do you maximumally expect?
             includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
             M5L5res = 10, #resolution in minutes of M5 and L5 calculation
             winhr = c(5,10), # size of M5 and L5 (5 hours by default)
             
             qlevels = c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
             qwindow=c(0,24), #window over which to calculate quantiles
             ilevels = c(seq(0,300,by=25),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             iglevels = TRUE, # intensitygradient levels
             mvpathreshold =c(70), #MVPA (moderate and vigorous physical activity threshold
             bout.metric = 4,
             # #-------------------------------
             # # Part 3 parameters:
             # #-------------------------------
             # # Key functions: Sleep detection
             # timethreshold= c(5), #10
             # anglethreshold=5,
             # ignorenonwear = TRUE, # if TRUE non-wear is not detected as sleep (if FALSE then it will work with imputed data)
             # constrain2range = TRUE,
             # do.part3.pdf = TRUE,
             # #-------------------------------
             # # Part 4 parameters:
             # #-------------------------------
             # # Key functions: Integrating sleep log (if available) with sleep detection, storing day and person specific summaries of sleep
             # excludefirstlast = TRUE, # Exclude first and last night for sleep analysis?
             # includenightcrit = 16, # number of minimum valid hours in a day to attempt sleep analysis
             # def.noc.sleep = c(1),
             # # If sleep log is available:
             # loglocation= c(), # full directory and name of the log (if available, otherwise leave value as c() )
             # outliers.only = TRUE,
             # criterror = 4,
             # relyonguider = FALSE,
             # sleeplogidnum = TRUE, # Is the participant in the sleep log stored as a number (TRUE) or as a character (FALSE)
             # colid=1, #colomn in which the participant id or filename is stored
             # coln1=2, #column number for first day
             # do.visual = TRUE,
             # nnights = 9, #number of nights in the sleep log
             # includedaycrit.part5 = 2/3,
             # minimum_MM_length.part5 = 23,
             # #-------------------------------
             # # Part 5 parameters:
             # #-------------------------------
             # # Key functions: Merging physical activity with sleep analyses
             # excludefirstlast.part5 = FALSE,
             # threshold.lig = c(40), #threshold(s) for inactivity (can be more than one number)
             # threshold.mod = c(100), #threshold(s) for moderate activity (can be more than one number)
             # threshold.vig = c(400), #threshold(s) for vigorous activity (can be more than one number)
             # boutcriter = 0.8,
             # boutcriter.in = 0.9, #fraction of an inactivity bout that needs to be below the threshold (needs to be 1 number)
             # boutcriter.lig = 0.8, #fraction of an light activity bout that needs to be between the thresholds (needs to be 1 number)
             # boutcriter.mvpa = 0.8, #fraction of an light activity bout that needs to be above the threshold (needs to be 1 number)
             # boutdur.in = c(5,10,30), # duration of bouts to be calculated
             # boutdur.lig = c(5,10), # duration of bouts to be calculated
             # boutdur.mvpa = c(5,10), # duration of bouts to be calculated
             # timewindow = c("WW"), #, ,"MM"
             # save_ms5rawlevels = TRUE,
             # part5_agg2_60seconds= TRUE,
             #-----------------------------------
             # Report generation
             #-------------------------------
             # Key functions: Generating reports based on meta-data
             
             visualreport=FALSE,
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow=1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
time1 = Sys.time()

print(difftime(time1,time0))
