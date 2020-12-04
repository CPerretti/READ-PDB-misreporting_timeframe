calcErrLO<- function(fitNo, fitMis, setup, simOut = NULL, scenario = NULL, replicate = NULL){

  # Only include leave-out predictions where both models converged
  # exclude fit fails
  ind2keep0 <- which(sapply(fitNo$fits, class)    == "sam" & 
                       sapply(fitMis$fits, class) == "sam")
  
  # exclude non-convergences
  ind2keep1 <- ind2keep0[unlist(sapply(fitNo$fits[ind2keep0], 
                                       function (x) x[[6]][3])) != 1 &
                           unlist(sapply(fitMis$fits[ind2keep0], 
                                        function (x) x[[6]][3])) != 1
                         ]
  # exclude unrealistic fits that would be exluded by an analyst
  if (!is.null(simOut)) {
    ind2keep2 <- vector()
    for (i in ind2keep1) {
      if (mean(exp(fitNo$fits[[i]]$pl$logN) / 
               exp(simOut$trueParams$pl$logN)) < 1000 &
          mean(exp(fitMis$fits[[i]]$pl$logN) / 
               exp(simOut$trueParams$pl$logN)) < 1000) {
        ind2keep2 <- c(ind2keep2, i)
      }
    }  
  } else ind2keep2 <- ind2keep1
  
  
  
  fitNoAccept <- list()
  fitMisAccept <- list()
  fitNoAccept$fits    <- fitNo$fits[ind2keep2]
  fitMisAccept$fits   <- fitMis$fits[ind2keep2]
  
  for (i in 1:length(ind2keep2)) {
    fitNoAccept$fits[[i]]$leaveOutYears <- fitNo$leaveOutYears[[ind2keep2[i]]]
    fitMisAccept$fits[[i]]$leaveOutYears <- fitMis$leaveOutYears[[ind2keep2[i]]] 
    
    fitNoAccept$fits[[i]]$model <- fitNo$model
    fitMisAccept$fits[[i]]$model <- fitMis$model
  }
  
  
  
  errNoSurvey <- pmap_dfr(.l = list(fitNoAccept$fits, 
                                    rep(list(simOut), length(ind2keep2)), 
                                    rep(list(setup), length(ind2keep2))),
                            .f = calcSurveyError, 
                          scenario = scenario, replicate = replicate)
  errMisSurvey <- pmap_dfr(.l = list(fitMisAccept$fits, 
                                     rep(list(simOut), length(ind2keep2)), 
                                     rep(list(setup), length(ind2keep2))),
                           .f = calcSurveyError, 
                           scenario = scenario, replicate = replicate)
  
  err0 <- rbind(errNoSurvey, errMisSurvey)
  
  # Calculate error on true variables only if this is simulated data
  if (!is.null(simOut)) {
    errNoRe  <- map2_dfr(fitNoAccept$fits, rep(list(simOut), length(ind2keep2)), calcReTsError) 
    errNoCSSB <- map2_dfr(fitNoAccept$fits, rep(list(simOut), length(ind2keep2)), calcCSSBError)
    errMisRe  <- map2_dfr(fitMisAccept$fits, rep(list(simOut), length(ind2keep2)), calcReTsError) 
    errMisCSSB <- map2_dfr(fitMisAccept$fits, rep(list(simOut), length(ind2keep2)), calcCSSBError)  
    
    err0 <- rbind(err0,
                  errNoRe, errNoCSSB,
                  errMisRe, errMisCSSB)
  }
  
  
  err <- 
    err0 %>%
    left_join({err0 %>%
        filter(variable == "S") %>%
        mutate(predictMisreporting95 = ifelse((exp(log(fit) - 1.96 * sdLog) < 1)  &  
                                                (exp(log(fit) + 1.96 * sdLog) > 1), 
                                              "No misreporting", "Yes misreporting"),
               trueMisreporting = ifelse(tru == 1, "No misreporting", "Yes misreporting"))})
  
  if (!is.null(simOut)) {
  err <-
    err %>%
    mutate(scenario = ifelse(scenario == "rw", "random walk", scenario),
           scenario = paste(scenario, "scenario"),
           scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                   "fixed scenario",
                                                   "random walk scenario",
                                                   "uniform random scenario",
                                                   "misspecified M scenario")))
  }
  
  return(err)
}