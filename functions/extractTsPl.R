## Extract random effect timeseries and sd's #######################
extractTsPl <- function(fit) {
  model <- deparse(substitute(fit))
  indRe <- which(names(fit$pl) %in% c("logN", "logF", "logS"))
  
  tsPl <- data.frame()
  
  for (h in indRe) {
    varName <- substr(names(fit$pl[h]), 4, 999)
    yearFit <- if (varName == "S") {
      fit$conf$keyScaledYears
    } else {as.numeric(fit$data$years)}
    
    rownames(fit$pl[[h]]) <- paste0("fit.", 1:nrow(fit$pl[[h]]))
    sdLog <- fit$plsd[[h]]
    rownames(sdLog) <- paste0("sdLog.", 1:nrow(fit$pl[[h]]))
    
    tsPl <-
      rbind(tsPl,
            fit$pl[[h]] %>%
              t() %>%
              exp %>%
              as.data.frame() %>%
              cbind(sdLog %>%
                      t() %>%
                      as.data.frame()) %>%
              dplyr::mutate(year = yearFit) %>%  
              tidyr::gather(variable, N, -year) %>%
              tidyr::separate(variable, c("source", "age")) %>%
              tidyr::spread(source, N) %>%
              dplyr::mutate(age = as.numeric(age),
                            variable = varName,
                            fit025 = exp(log(fit) - 1.96 * sdLog),
                            fit05  = exp(log(fit) - 1.64 * sdLog),
                            fit25  = exp(log(fit) - 0.674 * sdLog),
                            fit75  = exp(log(fit) + 0.674 * sdLog),
                            fit95  = exp(log(fit) + 1.64 * sdLog),
                            fit975 = exp(log(fit) + 1.96 * sdLog)))
  }
  return(tsPl)
}