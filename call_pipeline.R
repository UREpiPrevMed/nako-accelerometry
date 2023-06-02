#!/usr/bin/Rscript

options(echo = TRUE)
args = commandArgs(TRUE)
if (length(args) > 0) {
  for (i in 1:length(args)) {i
    tmp = gsub(pattern = "---", replacement = "/", x = args[[i]])
    tmp2 = unlist(strsplit(tmp, "="))
    assign(tmp2[1], as.character(tmp2[2]))
  }
}
source(paste0(rootdir, "/pipeline.R"))
library("GGIR")
pipeline(outputdir)
