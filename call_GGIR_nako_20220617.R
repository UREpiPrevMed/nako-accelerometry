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
options(echo = TRUE)
path = commandArgs(TRUE)[1]

library(devtools)
library(Rcpp)
library(R.utils)
library(GGIR)

datadir = path
tmp <- unlist(strsplit(path, split = "/", fixed = T))
outputdir = paste(paste(tmp[-length(tmp)], collapse = "/"), "ggir_out", sep = "/") 

time0 = Sys.time()

g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode = c(1), #specify above
             datadir = datadir, #specify above
             outputdir = outputdir, #specify above
             do.report = c(), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             overwrite = T, #overwrite previous milestone data?
             do.parallel = F,
             idloc = 1, #id location (1 = file header, 2 = filename)
             print.filename = TRUE, 
             storefolderstructure = TRUE,
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
             do.bfx = FALSE, #TRUE,
             do.bfy = FALSE, #TRUE,
             do.bfz = FALSE, #TRUE,
             do.anglex = TRUE, 
             do.angley = TRUE, 
             do.anglez = TRUE,
             lb = 0.3,
             hb = 20,
             n = 4,
             chunksize = 0.5, #size of data chunks to be read (value = 1 is maximum)
             printsummary = TRUE,
             visualreport = FALSE,
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
time1 = Sys.time()

print(difftime(time1, time0))
