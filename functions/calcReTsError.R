## Calculate random effect timeseries error #######################
calcReTsError <- function(fitSim, simOut) {
  datTrue <- extractTsPl(fitSim, simOut) %>% rename(tru = fit)
  
  datFit <- extractTsPl(fitSim)
  
  errRe <- 
    datFit %>%
    left_join(datTrue) %>%
    dplyr::select(year, age, fit, sdLog, variable, tru) %>%
    mutate(error = (fit - tru),
           abs_error = abs(error),
           error_pc = 100 * (fit - tru) / tru,
           abs_error_pc = abs(error_pc),
           decile = ceiling(10 * pnorm(q    = log(tru), 
                                       mean = log(fit), 
                                       sd   = sdLog)),
           scenario = simOut$sim_label$scenario,
           replicate = simOut$sim_label$replicate,
           model = fitSim$model)
  
  # If the fit is a leave-out fit, calc error on leave-out years only
  if(!is.null(fitSim[["leaveOutYears"]])) {
    errRe <- errRe %>% 
      filter(year %in% fitSim[["leaveOutYears"]]) %>%
      mutate(leaveOutYears = as.character(list(fitSim[["leaveOutYears"]])))
  }
  
  errRe$fleet <- NA
  
  return(errRe)
  }