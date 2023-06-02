pipeline = function(outputdir) {
  # Input argument: outputdir
  #
  # The path to a directory.
  # This directory is assumed to contain a folder named "output_ggir" structured as the typical
  # output directory created by R package GGIR with inside the following folder structure:
  #   output_ggir/meta/basic
  #   output_ggir/results/QC
  # the folder basic is assumed to be filed with the milestone data produced by GGIR part 1
  # The other folders are empty
  
  outdir2 = outputdir
  outputdir = paste0(outputdir,"/output_ggir")
  
  # check that outputdir has meta and results folder
  # create aggregates folder
  aggregates_folder =  paste0(outputdir, "/meta/aggregated_data")
  if (!dir.exists(aggregates_folder)) dir.create(aggregates_folder)
  
  expanded_results_folder = paste0(outputdir, "/results_expanded")
  if (!dir.exists(expanded_results_folder)) dir.create(expanded_results_folder)
  path.ms2 = paste0(outputdir, "/meta/ms2.out")
  if (!dir.exists(path.ms2)) dir.create(path.ms2)
  print(path.ms2)
  print(outdir2)
  #=====================================================================================
  # This part of the script does:
  # - apply GGIR part 2 to previously generated GGIR part1 milestone data
  # - if script sees multiple cores it will process multiple files in parallel
  # - generate GGIR part 2 report
  GGIR(#=======================================
               # INPUT NEEDED:
               #-------------------------------
               # General parameters
               #-------------------------------
               mode = c(2), #specify above
               datadir = "D:/test/ggir", #only used to pass check that data folder name matches output dir
               studyname = "ggir",
               outputdir = outdir2, #specify above
               do.report = c(2), #for what parts does and report need to be generated? (option: 2, 4 and 5)
               f0 = c(), #specify above
               f1 = c(), #specify above
               overwrite = TRUE, #overwrite previous milestone data?
               do.parallel = TRUE,
               idloc = 1, #id location (1 = file header, 2 = filename)
               print.filename = TRUE,
               storefolderstructure = TRUE,
               # data_cleaning_file = data_cleaning_file,
               desiredtz = "Europe/Berlin",
               #-------------------------------
               # Part 1 parameters:
               #-------------------------------
               # Key functions: reading file, auto-calibration, and extracting features
               windowsizes = c(5,900,3600), #Epoch length, non-wear detection resolution, non-wear detection evaluation window
               do.enmo = TRUE, #Needed for physical activity analysis
               do.mad = TRUE,
               do.bfen = FALSE, #TRUE,
               do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE, #TRUE,
               do.anglex = TRUE, do.angley = TRUE, do.anglez = TRUE,
               lb = 0.3, hb = 20, n = 4,
               chunksize = 1, #size of data chunks to be read (value = 1 is maximum)
               printsummary = TRUE,
               #-------------------------------
               # Part 2 parameters:
               #-------------------------------
               # Key functions: Non-wear detection, imputation, and basic descriptives
               strategy = 4, #Strategy (see tutorial for explanation)
               ndayswindow = 7, #only relevant when strategy = 3
               hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
               hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
               max_calendar_days = 8, # How many DAYS of measurement do you maximumally expect?
               includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
               M5L5res = 10, #resolution in minutes of M5 and L5 calculation
               winhr = c(5, 10), # size of M5 and L5 (5 hours by default)
               
               qlevels = c(c(1380/1440), c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
               qwindow = c(0,24), #window over which to calculate quantiles
               ilevels = c(seq(0, 360, by = 10), 8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
               iglevels = TRUE, # intensitygradient levels
               mvpathreshold = c(20, 30, 40,
                                 60, 65, 70, 75, 80, 90, 100, 110,
                                 170,
                                 240, 250, 260, 265, 270, 280,
                                 350,
                                 400, 410, 415, 420, 430), #MVPA (moderate and vigorous physical activity threshold
               bout.metric = 6,
               boutcriter = 0.8, # fraction of bout that needs to meet the threshold criteria
               mvpadur = c(1, 5, 10),
               #-----------------------------------
               # Report generation
               #-------------------------------
               # Key functions: Generating reports based on meta-data
               visualreport = FALSE,
               dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
               viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
  #=====================================================================================
  # This part of the script does:
  # - load GGIR part 2 milestone data, extract lying time variables, estimate bed time window per person
  # - store 15 minute aggregate data in a new RData milestone file
  # - if script sees multiple cores it will process multiple files in parallel
  # - at the end of the script it will run function create_summary, which:
  #   - tidies up and adds a few extra variables to part2 csv reports and QC report
  #   - combines all new milestone data (15 minute) in data.frame
  #   - aggregates the time series object
  f0 = 1
  f1 = length(dir(paste0(outputdir, "/meta/basic")))
  print(paste0("f1 ", f1))
  #===================================================================
  # Time series within a day with flexible resolution from 5 seconds to 12 hours
  #===================================================================
  cat('\n Time series')
  resolution = 15 # 15 minute resolution
  N = 1440 / resolution
  # identify files to load
  ms2f = dir(path.ms2, full.names = TRUE)
  print(path.ms2)
  print("head of ms2f")
  print(ms2f[1:4])
  # define function to load and aggregate the data
  load_and_aggregate = function(x, resolution) {
    load(x)
    CORREL = lying_df = outofbed = tobed = M2 = c()
    extract_id = function(x) {
      if (is.character(x)  == TRUE) {
        tmp = unlist(strsplit(x," |[.]|RAW"))[1]
        tmp2 = unlist(strsplit(tmp,"eta_"))
        if (length(tmp2) > 1) {
          id = as.numeric(tmp2[2])
        } else {
          id = as.numeric(tmp)
        }
      } else {
        cat(paste0("\nid not recognised ",x))
        id = 0
      }
      return(id)
    }
    ID  = extract_id(as.character(SUM$summary$filename))
    axis_of_interest = as.numeric(SUM$summary$if_hip_long_axis_id) # the axis with strongest laged 24 hour correlation
    if (length(axis_of_interest) == 1) {
      if (is.na(axis_of_interest) == FALSE) {
        colaxisinterest = grep(x = colnames(IMP$metashort), pattern = "angle", value = FALSE)[axis_of_interest]
        colacc = grep(x = colnames(IMP$metashort), pattern = "time|angle", invert = TRUE, value = FALSE)
        IMP$averageday = IMP$averageday[,c(colaxisinterest - 1, colacc - 1)]
        # Find correlation between first half and second half of recording
        B = IMP$metashort$ENMO
        N1 = length(B)
        N2 = floor(N1 / 2)
        N3 = min(N2, 5 * 12 * 1440)
        sel1 = 1:N3
        sel2 = (N2 + 1):(N2 + N3)
        CC = ccf(x = B[sel1],y = B[sel2], lag.max = 3 * 60 * 12, plot = FALSE)
        CORREL = CC$acf[which.max(CC$acf)]
        # aggregate per 15 minutes
        M = as.data.frame(cbind(floor((0:(nrow(IMP$averageday) - 1)) / (12 * resolution)), IMP$averageday))
        M2 = aggregate(x = M, by = list(M$V1), FUN = mean) #data is now aggregated per 15 minutes
        rm(M)
        M2[,1] = ID
        M2 = M2[, -2]
        colnames(M2) = c("ID", "angle_long_axis", colnames(IMP$metashort)[colacc])
        # Flip the angle if the average is not positive
        M2$flipped = 0
        if (mean(M2$angle_long_axis) < 0) {
          M2$angle_long_axis = -M2$angle_long_axis
          M2$flipped = 1
        }
        # Add time column (index of 1:96)
        # The averageday object is expressed relative to start time of recording, so we need to reset this to midnight
        starttime = as.POSIXlt(IMP$metashort$timestamp[1], format = "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Berlin")
        dayminute = (as.numeric(format(starttime, "%H"))*60) + as.numeric(format(starttime, "%M"))
        start_index = ceiling((dayminute + 1) / resolution)
        if (start_index != 1) {
          tsi = c(start_index:N,1:(start_index - 1))
        } else {
          tsi = 1:N
        }
        if (nrow(M2) != N) {
          M2 = c()
        } else {
          M2$time =  tsi
        }
        M2 = M2[order(M2$time), ]
        # Detect transition time in angle:
        #-------------------------------------------------
        # Extract day specific estimates of angle
        # add calendar date
        S = IMP$metashort[,c("timestamp", colnames(IMP$metashort)[3 + axis_of_interest])]
        ws3 = IMP$windowsizes[1]
        S$timestamp = as.POSIXlt(S$timestamp, format = "%Y-%m-%dT%H:%M:%S%z",tz = "Europe/Berlin")
        S$Date = as.Date(S$timestamp)
        udays = unique(S$Date)
        Ndays = length(udays)
        if (Ndays > 0) {
          lying_df = data.frame(ID = rep(0,Ndays), date = udays, dur_lying = rep(0, Ndays), 
                                Nepochs_lying = rep(0, Ndays))
          for (ui in 1:Ndays) {
            Sday = S[which(S$Date == udays[ui]),]
            lying_df$ID = ID
            # calculate time between -20 and 20 degrees
            Lindices = which(Sday[,2] < 20 & Sday[,2] > -20) # indices for lying
            lying_df$dur_lying[ui] = length(Lindices) / (60/ws3) # in minutes
            lying_df$Nepochs_lying[ui] = length(which(abs(diff(Lindices)) > 1))
          }
        }
        #------------------------------------------
        if (mean(M2$angle_long_axis) < 0) {
          M2$angle_long_axis = -M2$angle_long_axis
          M2$flipped = 1
        }
        # smooth signal
        NV = nrow(M2)
        M2$angle_long_axis = (M2$angle_long_axis + M2$angle_long_axis[c(2:NV, 1)] + 
                                M2$angle_long_axis[c(3:NV, 1:2)] +
                                M2$angle_long_axis[c(NV, 1:(NV - 1))] + 
                                M2$angle_long_axis[c(NV - 1, NV, 1:(NV - 2))]) / 5
        # find angle threshold were there are only two line crossing starting at 20 and moving up
        # if this is not found below 65 then consider these undetectable
        Ncrossing = 0
        AngleThres = 20
        outofbed = tobed = NULL
        while (Ncrossing != 2) {
          tobed = which(M2$angle_long_axis[1:(nrow(M2) - 1)] >= AngleThres &
                          M2$angle_long_axis[2:nrow(M2)] < AngleThres)
          outofbed = which(M2$angle_long_axis[1:(nrow(M2) - 1)] < AngleThres &
                             M2$angle_long_axis[2:nrow(M2)] >= AngleThres)
          Ncrossing = length(outofbed) + length(tobed)
          if (AngleThres > 40) {
            outofbed = tobed =  NULL
            Ncrossing  = 2
          }
          if (Ncrossing != 2) {
            AngleThres = AngleThres + 5
          } else {
            Ncrossing = 2
          }
        }
        if (length(outofbed) == 1) {
          outofbed = (outofbed - 1) / (60/resolution) # convert to hours
        }
        if (length(tobed) == 1) {
          tobed = (tobed - 1) / (60/resolution) # convert to hours
        }
      }
    }
    invisible(list(aggdata = M2, outofbed = outofbed, tobed = tobed, lying_df = lying_df,
                   CORREL = CORREL, ID = ID))
  }
  print("apply load_and_aggregate function")
  # apply the above function to all the RData files found
  f1 = length(ms2f)
  # functions2passon = "load_and_aggregate"
  errhand = 'stop'
  fe_dopar = foreach::`%dopar%`
  fe_do = foreach::`%do%`
  do.parallel = TRUE
  cores = parallel::detectCores()
  Ncores = cores[1]
  maxNcores = 20
  if (Ncores > 3) {
    if (length(maxNcores) == 0) maxNcores = Ncores
    Ncores2use = min(c(Ncores - 1, maxNcores))
    cl <- parallel::makeCluster(Ncores2use) #not to overload your computer
    doParallel::registerDoParallel(cl)
  } else {
    cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
    do.parallel = FALSE
  }
  
  print("------------------------------------")
  print("load and aggrergate:....")
  print(paste0("f0=",f0," f1=",f1))
  j = 0 # declare i because foreach uses it, without declaring it
  `%myinfix%` = ifelse(do.parallel, fe_dopar, fe_do) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
  output_list = foreach::foreach(j  = f0:f1,
                                 # .export=functions2passon,
                                 .errorhandling = errhand) %myinfix% {
                                   tryCatchResult = tryCatch({
                                     # for (j in f0:f1) {
                                     progress = round((j / (f1 - f0 + 1)) * 100, digits = 2)
                                     if ((j/100) == round(j / 100)) cat(paste0(" ", progress))
                                     filename = paste0(aggregates_folder, "/", basename(ms2f[j]))
                                     if (!file.exists(filename)) {
                                       ms2a = load_and_aggregate(ms2f[j], resolution = resolution)
                                       save(ms2a, file = filename)
                                       rm(ms2a)
                                     }
                                     # } # for loop
                                   }) # END try Catch
                                   return(tryCatchResult)
                                 }
  
  print("run script_create_summary")
  # person summary
  fns =  grep(x = dir(outputdir, full.names = T, recursive = TRUE), pattern = "part2_summa", value = TRUE)
  
  read_and_trim = function(x) {
    P2 = read.csv(x)
    cols2remove = grep(x = colnames(P2), 	pattern = "fullRecord|WWE|WWD|_ig_")
    P2 = P2[,-cols2remove]
    return(P2)
  }
  
  person_summary = do.call("rbind", lapply(fns, FUN = read_and_trim)) #function(files) { read.csv(files)}))
  person_summary = person_summary[!duplicated(person_summary),]
  # QC summary
  qcfiles =  grep(x = dir(outputdir, full.names = T, recursive = TRUE), pattern = "ata_quality_repo", value = TRUE)
  read_and_trim2 = function(x) {
    QC = read.csv(x)
    QC = QC[,c("filename", "file.corrupt", "file.too.short", "scale.x", "scale.y", "scale.z", "offset.x", "offset.y", "offset.z")]
    return(QC)
  }
  qc_summary = do.call("rbind", lapply(qcfiles, FUN = read_and_trim2)) #function(files) { read.csv(files)}))
  qc_summary = qc_summary[!duplicated(qc_summary),]
  # daysummary
  fns =  grep(x = dir(outputdir, full.names = T, recursive = TRUE), pattern = "part2_daysumma", value = TRUE)
  read_and_trim_day = function(x) {
    P2 = read.csv(x)
    cols2remove = grep(x = colnames(P2), 	pattern = "_ig_")
    P2 = P2[,-cols2remove]
    return(P2)
  }
  
  day_summary = do.call("rbind", lapply(fns, FUN = read_and_trim_day))
  day_summary = day_summary[!duplicated(day_summary),]
  
  #===================================================================
  # Per person
  #====================================================
  cat('\n Per person')
  print(grep(pattern = "MVPA", x = colnames(person_summary), value = TRUE))
  
  person_summary = person_summary[,c("filename","device_sn","clipping_score","samplefreq","meas_dur_dys","complete_24hcycle",
                                     "meas_dur_def_proto_day","wear_dur_def_proto_day",
                                     "calib_err","AD_mean_ENMO_mg_0.24hr","AD_mean_MAD_mg_0.24hr" ,"N.valid.WEdays","N.valid.WKdays",
                                     grep(pattern = "MVPA", x = colnames(person_summary), value = TRUE))]
  
  print(length(which(person_summary$calib_err <= 0.01 &
                       person_summary$N.valid.WEdays >= 1 &
                       person_summary$N.valid.WKdays >= 1)))
  print(length(which(person_summary$calib_err <= 0.01 &
                       (person_summary$N.valid.WEdays + person_summary$N.valid.WKdays) > 2)))
  
  extract_id = function(x) {
    
    if (is.character(x)  == TRUE) {
      tmp = unlist(strsplit(x," |[.]|RAW"))[1]
      tmp2 = unlist(strsplit(tmp,"eta_"))
      if (length(tmp2) > 1) {
        id = as.numeric(tmp2[2])
      } else {
        id = as.numeric(tmp)
      }
    } else {
      cat(paste0("\nid not recognised ",x))
      id = 0
    }
    return(id)
  }
  person_summary$ID = as.numeric(sapply(X = as.character(person_summary$filename),FUN =  extract_id))
  
  person_summary$compliance = person_summary$wear_dur_def_proto_day / person_summary$meas_dur_def_proto_day
  person_summary$tobed = person_summary$outofbed = person_summary$CORREL = NA
  
  # # identify recordings that need to be excluded
  # ids_2_exclude = person_summary$ID[which(person_summary$calib_err > 0.01 |
  #                                           person_summary$N.valid.WEdays == 0 |
  #                                           person_summary$N.valid.WKdays < 2)]
  
  # Object person_summary should now have all the key variables at person level:
  #
  # - compliance => fraction of data that is valid, so measurement that stops after 5 days is not a compliance problem
  # - meas_dur_def_proto_day => number of days recording according to protocol (counting days only after first midnight)
  # - N.valid.WEdays and N.valid.WKdays => to be used as exclusion criteria
  # - calibr_err => calibration error after auto-calibration attempt
  # - AD_mean_ENMO_mg_0.24hr => average ENMO per day
  # - AD_mean_MAD_mg_0.24hr => average MAD per day
  # - filename
  # - ID (extracted from filename)
  
  
  #===================================================================
  # QC summary # additional QC variables that should even capture corrupt
  # or recordings that are too short for processing
  #====================================================
  qc_summary$ID = as.numeric(sapply(X = as.character(qc_summary$filename),FUN =  extract_id))
  
  # Object qc_summary includes:
  # - filename
  # - ID
  # - file.corrupt (Boolean)
  # - file.too.short (Boolean)
  # - scale.x, scale.y, scale.y, ...
  #  offset.x, offset.y., offset.z => The calibration coefficients derived with auto-calibration
  #  these allows us to invesitgate whether calibration error drifted across repeated measurements
  
  #===================================================================
  # Per day
  #====================================================
  cat('\n Per day')
  CDS = colnames(day_summary)
  CN_of_interest = c(which(colnames(day_summary) %in% c("filename","weekday",
                                                        "N.valid.hours", "N.hours",
                                                        "mean_ENMO_mg_0.24hr",
                                                        "mean_MAD_mg_0.24hr", "calendar_date","N.valid.hours") == TRUE),
                     which(colnames(day_summary) == "X.0.10._ENMO_mg_0.24hr"):which(colnames(day_summary) == "X.360.8e.03._ENMO_mg_0.24hr"),
                     which(colnames(day_summary) == "X.0.10._MAD_mg_0.24hr"):which(colnames(day_summary) == "X.360.8e.03._MAD_mg_0.24hr"),
                     grep(pattern = "MVPA", x = colnames(day_summary)))
  day_summary = day_summary[,CN_of_interest]
  day_summary$ID = as.numeric(sapply(X = as.character(day_summary$filename),FUN =  extract_id))
  # if (length(ids_2_exclude) > 0) {
  #   day_summary = day_summary[which(day_summary$ID %in% ids_2_exclude == FALSE),]
  # }
  day_summary$calendar_date = as.POSIXlt(day_summary$calendar_date,format = "%Y-%m-%dT%H:%M:%S%z",
                                         tz = "Europe/Berlin")
  day_summary$calendar_date = as.Date(day_summary$calendar_date)
  day_summary$month = as.numeric(format(day_summary$calendar_date, "%m"))
  
  
  # Object day_summary should now have the key variables we need at day level:
  #
  # - N.valid.hours => number of valid hours in the day
  # - weekday
  # - calendar_date
  # - month
  # - mean_ENMO_mg_0.24hr and mean_MAD_mg_0.24hr => average acceleration per day
  # - X.... => these variables indicate time spent in various acceleration levels
  
  #===================================================================
  # Time series within a day with flexible resolution from 5 seconds to 12 hours
  #===================================================================
  cat('\n Time series')
  resolution = 15 # 15 minute resolution
  N = 1440 / resolution
  
  cat("\nLoad aggregated time series\n")
  # apply the above function to all the RData files found
  
  # Next line is too slow... this is why I have reimplemented it as a loop
  # ts = do.call("rbind", lapply(ms2f, FUN=load_and_aggregate, resolution)) #function(files) { read.csv(files)}))
  # cat(aggregated_data)
  ms2f = dir(path = aggregates_folder, recursive =  T, full.names = T)
  cat(paste0("ms2f has length: ", length(ms2f)))
  
  cat("\n1.  Load one aggregated time series to initialise output data.frame ts\n")
  cat(paste0("\n",ms2f[1],"\n"))
  load(ms2f[1])
  cnt = 1
  while (cnt <= length(ms2f)) {
    cat("\n2. data loaded")
    if (length(ms2a) != 4) {
      cat(paste0("\n",ms2f[cnt],"\n"))
      load(ms2f[cnt])
      if (!is.null(ms2a$aggdata)) {
        break()
      }
      cnt = cnt + 1
    }
  }

  cat("\n")
  aggdata = ms2a$aggdata
  cat("\n")
  ts = as.data.frame(matrix(NA, length(ms2f) * N, ncol(aggdata)))
  colnames(ts) = colnames(aggdata)
  
  cat("\n3. Initialise extra columns for day_summary\n")
  
  lying_names = c("dur_lying", "Nepochs_lying")
  day_summary$dur_lying = 0
  day_summary$Nepochs_lying = 0
  print("check data columns exist")
  print(lying_names %in% colnames(day_summary))
  showprogressat = round(length(ms2f) / 20)
  cat(paste0("\n Progres...",Sys.time()," "))
  for (j in 1:length(ms2f)) {
    if (j / showprogressat == round(j/showprogressat)) cat(paste0(" ",round((j/length(ms2f)) * 100, digits = 1), "%"))
    if (file.exists(ms2f[j])) {
      load(ms2f[j])
      aggdata = ms2a$aggdata
      lying_df = ms2a$lying_df
      CORREL = ms2a$CORREL
      if (length(aggdata) > 0) {
        if (length(aggdata) > 0) {
          if (nrow(aggdata) == N) {
            ts[(((j - 1) * N) + 1):(j * N),] = aggdata
            idrow = which(person_summary$ID == aggdata$ID[1])
            if (!is.null(ms2a$tobed)) person_summary$tobed[idrow] = ms2a$tobed
            if (!is.null(ms2a$outofbed)) person_summary$outofbed[idrow] = ms2a$outofbed
            if (length(CORREL) == 1) {
              if (!is.null(CORREL)) person_summary$CORREL[idrow] = CORREL
            }
          }
        }
        udays = lying_df$date
        Ndays = length(udays)
        for (ui in 1:Ndays) {
          rowind = which(day_summary$ID == lying_df$ID[ui] &
                           day_summary$calendar_date == udays[ui])
          if (length(rowind) > 0) {
            day_summary[rowind , lying_names] = lying_df[ui, lying_names]
          }
        }
      }
    }
  }
  ts = ts[which(is.na(ts$ID) == FALSE),] # ignore the files that were skipped
  cat("\n4. Tidy up loaded and aggregated time series\n")
  # rescale acceleration from g to mg
  print(str(ts))
  print(head(ts))
  ts$ENMO = ts$ENMO * 1000
  ts$MAD = ts$MAD * 1000
  # Define functions related to aggregating across individuals
  myfun = function(x, prob){
    return(quantile(x, probs = prob, na.rm = T))
  }
  addsuffix = function(x, suf) {
    cn = colnames(x)
    select = which(cn != "Group.1")
    cn = paste0(cn[select], suf)
    colnames(x)[select] = cn
    return(x)
  }
  cat("\n5. Aggregate time series across individuals to create summary plot\n")
  PBS = c(0.025, 0.25, 0.5, 0.75, 0.975) # percentiles to extract
  ts0 = aggregate(ts[,-which(colnames(ts) %in% c("flipped", "ID", "time") == TRUE)], 
                  by = list(ts$time), FUN = mean)
  ts1 = aggregate(ts[,-which(colnames(ts) %in% c("flipped", "ID", "time") == TRUE)],
                  by = list(ts$time), FUN = sd, na.rm = T)
  ts_agg = merge(ts0, ts1, by = c("Group.1"), suffixes = c("_mean","_sd"))
  for (i in 1:length(PBS)) {
    ts2 = aggregate(ts[,-which(colnames(ts) %in% c("flipped", "ID") == TRUE)],
                    by = list(ts$time), FUN = myfun, prob = PBS[i])
    ts2 = ts2[,-which(colnames(ts2) == "time")]
    ts2 = addsuffix(ts2, paste0("_q", PBS[i] * 1000))
    ts_agg = merge(ts_agg, ts2, by = c("Group.1"))
  }
  colnames(ts_agg)[1] = c("time")
  print(head(ts_agg))
  ts$time = ((ts$time / N) * 24) #- 1
  ts_agg$time = ((ts_agg$time / N) * 24) #- 1
  # Object ts represents the typical 24 hour time series for each of the individual recordings with columns:
  # - ID
  # - ENMO
  # - MAD
  # - angle_long_axis => angle of the longitudinal axis (anatomical position) relative to horizontal
  # - flipped => 0 if the angle was flipped, 1 if it was not flipped
  # - time => time in the day (expressed as a number between 0 and 24, where [0,1) is the first hour of the day
  
  # Object ts_agg represents the typical 24 time series across all 24 individuals, including mean and a series of percentiles
  # - time => time in the day (expressed as a number between 0 and 24, where [0,1) is the first hour of the day
  # - ENMO_mean and MAD_mean => average acceleration for this time point in the day across all recordings
  # - angle_long_axis_mean => average angle
  # - ENMO_sd MAD_sd angle_long_axis_sd => standard deviation
  # - ENMO_q25 ... => 2.5th percentile in ENMO distributution
  # - MAD_q250 ... => 25th percentile in MAD distributution
  # - angle_long_axis_q750 ... => 75th percentile in angle distributution
  
  cat("\n6. Plot timeseries to pdf file\n")
  pdf(file = paste0(expanded_results_folder,"/timeseries_plot.pdf"))
  
  plot(ts_agg$time, ts_agg$angle_long_axis_q500, type = "l", ylim = c(-90, 90), 
       xlab = "time", ylab = "longitudinal angle")
  lines(ts_agg$time, ts_agg$angle_long_axis_q25, type = "l", lty = 3)
  lines(ts_agg$time, ts_agg$angle_long_axis_q250, type = "l", lty = 2, col = "blue")
  lines(ts_agg$time, ts_agg$angle_long_axis_q750, type = "l", lty = 2, col = "blue")
  lines(ts_agg$time, ts_agg$angle_long_axis_q975, type = "l", lty = 3)
  
  plot(ts_agg$time, ts_agg$ENMO_q500, type = "l", ylim = c(-20, 100), xlab = "time", ylab = "ENMO")
  lines(ts_agg$time, ts_agg$ENMO_q25, type = "l", lty = 3)
  lines(ts_agg$time, ts_agg$ENMO_q250, type = "l", lty = 2, col = "blue")
  lines(ts_agg$time, ts_agg$ENMO_q750, type = "l", lty = 2,col = "blue")
  lines(ts_agg$time, ts_agg$ENMO_q975, type = "l", lty = 3)
  
  plot(ts_agg$time, ts_agg$MAD_q500, type = "l", ylim = c(-20, 100), xlab = "time", ylab = "MAD")
  lines(ts_agg$time, ts_agg$MAD_q25, type = "l", lty = 3)
  lines(ts_agg$time, ts_agg$MAD_q250, type = "l", lty = 2, col = "blue")
  lines(ts_agg$time, ts_agg$MAD_q750, type = "l", lty = 2, col = "blue")
  lines(ts_agg$time, ts_agg$MAD_q975, type = "l", lty = 3)
  dev.off()
  
  cat("\n7. Save all output to csv files")
  day_summary[is.na(day_summary)] <- ""
  person_summary[is.na(person_summary)] <- ""
  person_summary[is.na(person_summary)] <- ""
  print(paste0(expanded_results_folder, "/daysummary.csv"))
  write.csv(day_summary, file = paste0(expanded_results_folder, "/day_summary.csv"), row.names = F)
  write.csv(person_summary, file = paste0(expanded_results_folder,"/person_summary.csv"), row.names = F)
  write.csv(qc_summary, file = paste0(expanded_results_folder, "/qc_summary.csv"), row.names = F)
  write.csv(ts_agg, file = paste0(expanded_results_folder, "/ts_agg.csv"), row.names = F)
  write.csv(ts, file = paste0(expanded_results_folder, "/timeseries.csv"), row.names = F)
  cat(paste0("\nTime completed ",Sys.time()))
}
