calcRes <- function(fit, sim_label) {
    
    if ("keyVarS" %in% names(fit$conf)) { # if fit used rw for S
      res <- stockassessment2:::residuals.sam(fit, trace = FALSE)
    } else {
      res <- stockassessment:::residuals.sam(fit, trace = FALSE)
    }
    
    
    data.frame(year = res$year,
               fleet = res$fleet,
               fleetName = attr(res, "fleetNames")[res$fleet],
               age = paste0("age-",res$age),
               observation = res$observation,
               nll = res$nll,
               grad = res$grad,
               mean = res$mean,
               residual = res$residual,
               scenario = sim_label$scenario,
               replicate = sim_label$replicate)
    
  }