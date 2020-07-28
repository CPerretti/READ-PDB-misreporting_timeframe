extractFitVsObs <- function(fit, model = fit$model) {
  logResidualCatchFit <- fit$sdrep$value[names(fit$sdrep$value) == "logCatch"]
  dfFitVsObs <- 
    data.frame(fit$data$aux,
               fleetName = attr(fit$data, "fleetNames")[fit$data$aux[,"fleet"]],
               logobs = fit$data$logobs,
               obs = exp(fit$data$logobs),
               logobsFit = fit$rep$predObs,
               obsFit =exp(fit$rep$predObs),
               model = model) %>%
    mutate(age = paste0("age-", age))
}