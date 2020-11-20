fit <- function(dat, conf, par, modelName, ...) {
  if (is.null(par$logS)) {
    fit <- sam.fit(dat, conf, par, ...) 
  } else {
    fit <- sam.fit_cp(dat, conf, par, ...)
  }
  fit$model <- modelName
  
  return(fit)
}