#----------------------------------------------
# program: appendbatches.R
# description: appends all GGIR summary stats
#              to a respective overall file
# date: 30.01.2023
# author: H.Baurecht
#---------------------------------------------

path = "/.../nako_ggir_200k/"
outpath = "/.../nako_ggir_200k_append_results/"

files <- system(paste("ls ", path, sep = ""), intern = T)

N <- as.integer(system(paste("ls ", path, " | wc -l", sep = ""),
                       intern = T))
failed_files <- data.frame(matrix(ncol = 2, nrow = N))
colnames(failed_files) <- c("batch", "summaries")

for (i in 1:length(files)) {
  test <- as.integer(system(paste("ls ", path,
                                  files[i], "/output_ggir/results_expanded | wc -l",
                                  sep = ""), intern = T))
  if (test == 0) {
    failed_files$batch[i] <- files[i]
    failed_files$summaries[i] <- "no"
  } else {
    failed_files$batch[i] <- files[i]
    failed_files$summaries[i] <- "yes"
    tmp_pers <- read.table(paste(path, files[i],
                                 "/output_ggir/results_expanded/person_summary.csv", sep = ""),
                           sep = ",", header = T)
    tmp_day <- read.table(paste(path, files[i],"/output_ggir/results_expanded/day_summary.csv", sep = ""),
                          sep = ",", header = T)
    tmp_qc <- read.table(paste(path, files[i],"/output_ggir/results_expanded/qc_summary.csv", sep = ""),
                         sep = ",", header = T)
    tmp_ts <- read.table(paste(path, files[i],"/output_ggir/results_expanded/timeseries.csv", sep = ""),
                         sep = ",", header = T)
    if ( i == 1) {
      person_summary <- tmp_pers
      day_summary <- tmp_day
      qc_summary <- tmp_qc
      timeseries <- tmp_ts
    } else {
      person_summary <- rbind(person_summary, tmp_pers)
      day_summary <- rbind(day_summary, tmp_day)
      qc_summary <- rbind(qc_summary, tmp_qc)
      timeseries <- rbind(timeseries, tmp_ts)
    }
  }
}

write.table(person_summary, file = paste(outpath, "/all_person_summary2.csv", sep = ""), quote = F, sep = ",")
write.table(day_summary, file = paste(outpath, "/all_day_summary2.csv", sep = ""), quote = F, sep = ",")
write.table(qc_summary, file = paste(outpath, "/all_qc_summary2.csv",sep = ""), quote = F, sep = ",")
write.table(timeseries, file = paste(outpath, "/all_timeseries2.csv",sep = ""), quote = F, sep = ",")
write.table(failed_files, file = paste(outpath, "/summaries_available.csv",sep = ""), quote = F, sep = ",")

  
