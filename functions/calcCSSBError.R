## Calculate observed time series error ###################
calcCSSBError <- function(fitSim, simOut) {

  # Fit catch and SSB
  df_CSSBfit <- 
    data.frame(variable = names(fitSim$sdrep$value),
               value = fitSim$sdrep$value,
               sd    = fitSim$sdrep$sd) %>%
    dplyr::filter(variable %in% c("logCatch", "logssb")) %>%
    dplyr::rename(sdLog = sd) %>%
    dplyr::mutate(fit = exp(value),
                  year = rep(fitSim$data$years, 2),
                  age = "total",
                  variable = as.character(variable),
                  variable = ifelse(variable == "logCatch", "catch", variable),
                  variable = ifelse(variable == "logssb", "ssb", variable)) %>%
    dplyr::select(year, age, fit, sdLog, variable)
  
  # True catch and SSB
  df_CSSBtru <- 
    simOut$Ctru_mt %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(total = rowSums(.),
                  year = fitSim$data$years) %>%
    tidyr::gather(age, Catch_mt, -year) %>%
    dplyr::rename(tru = Catch_mt) %>%
    dplyr::filter(age == "total") %>%
    dplyr::mutate(variable = "catch") %>%
    rbind(data.frame(tru = simOut$SSB,
                     year = names(simOut$SSB),
                     age = "total",
                     variable = "ssb")) %>%
    dplyr::mutate(year = as.integer(year)) %>%
    rbind({simOut$Cobs_mt %>%
        t() %>%
        as.data.frame() %>%
        dplyr::mutate(total = rowSums(.),
                      year = fitSim$data$years) %>%
        tidyr::gather(age, Catch_mt, -year) %>%
        dplyr::rename(tru = Catch_mt) %>%
        dplyr::filter(age == "total") %>%
        dplyr::mutate(variable = "catch_observed")})
  
  
  errCSSB <-
    df_CSSBfit %>%
    rbind({df_CSSBfit %>% 
        filter(variable == "catch") %>% 
        mutate(variable = "catch_observed")}) %>%
    dplyr::left_join(df_CSSBtru) %>%
    dplyr::mutate(error = fit - tru,
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
    errCSSB <- errCSSB %>% 
      filter(year %in% fitSim[["leaveOutYears"]]) %>%
      mutate(leaveOutYears = as.character(list(fitSim[["leaveOutYears"]])))
  }
  
  errCSSB$fleet <- NA
  
  return(errCSSB)
}