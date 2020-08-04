calcSurveyError <- function(fitSim, simOut = NULL, scenario = NULL, replicate = NULL) {
  
  if (!is.null(simOut)) {
    scenario <- simOut$sim_label$scenario
    replicate <- simOut$sim_label$replicate
  }
  
  
  errSurvey <- 
    data.frame(fitSim$data$aux,
               tru = fitSim$data$logobs %>% exp, # says tru but actually is obs
               fit = fitSim$rep$predObs %>% exp,
               sdLog = NA) %>%
    filter(fleet == 1) %>%
    select(-fleet) %>%
    mutate(error = fit - tru,
           abs_error = abs(error),
           error_pc = 100 * (fit - tru) / tru,
           abs_error_pc = abs(error_pc),
           decile = NA,
           scenario = scenario,
           replicate = replicate,
           model = fitSim$model,
           variable = "Survey")
  
  # If the fit is a leave-out fit, calc error on leave-out years only
  if(!is.null(fitSim[["leaveOutYears"]])) {
    errSurvey <- errSurvey %>% 
      filter(year %in% fitSim[["leaveOutYears"]]) %>%
      mutate(leaveOutYears = as.character(list(fitSim[["leaveOutYears"]])))
  }
  
  return(errSurvey)

}