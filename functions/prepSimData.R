## Prep simulation data to be fit by SAM ##################
prepSimData <- function(simOut) {
  
  # Convert survey data to SAM input format
  surveyIndex <- # some fleets are fishermen not surveys
    (1:simOut$trueParams$data$noFleets)[simOut$trueParams$data$fleetTypes == 2]
  
  surveys <-
    lapply(X = plyr::alply(simOut$Sobs_N[, , surveyIndex], 3, .dims = TRUE), t) %>%
    lapply(function(x) {colnames(x) <- sub(colnames(x), # Change colnames
                                           pattern = "simulated.", 
                                           replacement = ""); x}) %>%
    # Remove ages not in survey
    lapply(function(x) x[, which(colSums(x, na.rm = TRUE) != 0)]) %>%
    # Remove years (rows) without any data
    lapply(function(x) x[which(rowSums(x, na.rm = TRUE) != 0), ])
  
  for(i in 1:length(surveys)) { # set survey times to match real data
    attr(surveys[[i]], "time", rep(simOut$trueParams$data$sampleTimes[surveyIndex[i]],2))
  }
  
  # Export the simulated surveys to a text file, then import it
  # using read.ices().
  
  # First set survey file header
  write.table(rbind("Fake Fish Survey Data", 100 + length(surveys)), 
              file = "./wg_MGWG/state-space/simData/SIMDATA_survey.dat", sep = " ", 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  # Then append surveys
  for (i in 1:length(surveys)) { # loop over surveys
    header_survey <-
      rbind(names(surveys)[i], 
            paste(min(as.numeric(rownames(surveys[[i]]))),
                  max(as.numeric(rownames(surveys[[i]])))),
            paste(1, 1, simOut$trueParams$data$sampleTimes[surveyIndex[i]], 
                  simOut$trueParams$data$sampleTimes[surveyIndex[i]]),
            paste(min(colnames(surveys[[i]])), max(colnames(surveys[[i]]))))
    a_survey <- cbind(1, as.data.frame(surveys[[i]]))
    
    # Save it to file so it can be read in
    write.table(header_survey, 
                file = "./wg_MGWG/state-space/simData/SIMDATA_survey.dat", 
                sep = " ", 
                row.names = FALSE, col.names = FALSE, quote = FALSE,
                append = TRUE)
    
    write.table(a_survey, 
                file = "./wg_MGWG/state-space/simData/SIMDATA_survey.dat", 
                sep = " ",
                row.names = FALSE, col.names = FALSE, quote = FALSE,
                append = TRUE)
  }
  
  
  # Next, manipulate catch so it also can be read in by read.ices()
  catch <- t(simOut$Cobs_N) 
  colnames(catch) <- sub(colnames(catch), 
                         pattern = "simulated.", 
                         replacement = "")
  catch <- catch[, which(colSums(catch, na.rm = TRUE) != 0)]
  header_catch <-
    rbind("Fake Fish Total Catch Numbers at age (000s; combines all gear types)", 
          paste(1, 2),
          paste(min(as.numeric(rownames(catch))),
                max(as.numeric(rownames(catch)))),
          paste(min(colnames(catch)), max(colnames(catch))),
          "1")
  
  # Save it to file so it can be read in
  write.table(header_catch, 
              file = "./wg_MGWG/state-space/simData/SIMDATA_cn.dat", sep = " ", 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  write.table(catch, 
              file = "./wg_MGWG/state-space/simData/SIMDATA_cn.dat", sep = " ", 
              row.names = FALSE, col.names = FALSE, quote = FALSE,
              append = TRUE)
}