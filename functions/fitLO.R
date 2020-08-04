fitLO <- function(dat, conf, par, model, ...) {
  # k-fold cross-validation on survey data
  surveyYears <- unique(dat$aux[,"year"][dat$aux[,"fleet"] %in% c((1:dat$noFleets)[dat$fleetTypes == 2])])
  k = length(surveyYears) # THIS IS FOR LOO CV, REMOVE IF YOU WANT k-fold
  leaveOutYears <- split(surveyYears, cut(seq_along(surveyYears), k, labels = FALSE)) 
  fitLO <- list()
  for (i in 1:length(leaveOutYears)) {
    
    # Turn survey observations off in leave-out-years
    dat$logobs[dat$aux[,"year"] %in% leaveOutYears[[i]] & 
                 dat$aux[,"fleet"] %in% c((1:dat$noFleets)[dat$fleetTypes == 2])] <- NA
    
    if (is.null(par$logS)) {
      fitLO$fits[i] <- list(try(sam.fit(dat, conf, par, ...)))
    } else {
      fitLO$fits[i] <- list(try(sam.fit_cp(dat, conf, par, ...)))
    } 
    
  }
  fitLO$leaveOutYears <- leaveOutYears
  fitLO$model <- model

   
   return(fitLO)
}

