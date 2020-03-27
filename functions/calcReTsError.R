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
           model = ifelse(fitSim$conf$corFlagS %>% is.numeric, "random walk", NA),
           scenario = simOut$sim_label$scenario,
           replicate = simOut$sim_label$replicate)
  
  return(errRe)
  }