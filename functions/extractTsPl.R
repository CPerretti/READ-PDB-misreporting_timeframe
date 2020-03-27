## Extract random effect timeseries and sd's #######################
extractTsPl <- function(fit, simOut = NULL) {
  # if simOut is given this extracts true vals
  if(!is.null(simOut)) { 
    fit$pl <- simOut$trueParams$pl
  }
  
  model <- deparse(substitute(fit))
  indRe <- which(names(fit$pl) %in% c("logN", "logF", "logS"))
  
  tsPl <- data.frame()
  
  for (h in indRe) {
    varName <- substr(names(fit$pl[h]), 4, 999)
    yearFit <- if (varName == "S" & is.null(simOut)) {
      fit$conf$keyScaledYears
    } else { #if (varName == "S" & !is.null(simOut)) {
      #simOut$trueParams$conf$keyScaledYears
      #} else {
      as.numeric(fit$data$years)}
    
    rownames(fit$pl[[h]]) <- paste0("fit.", 1:nrow(fit$pl[[h]]))
    if(is.null(simOut)){
      sdLog <- fit$plsd[[h]]
      rownames(sdLog) <- paste0("sdLog.", 1:nrow(fit$pl[[h]]))
    }
    
    tsPl <-
      rbind(tsPl,
            fit$pl[[h]] %>%
              t() %>%
              exp %>%
              as.data.frame() %>%
              {if (is.null(simOut)) {
                  cbind(., sdLog %>%
                  t() %>%
                  as.data.frame())
                } else .} %>%
              mutate(year = yearFit) %>%  
              gather(variable, N, -year) %>%
              separate(variable, c("source", "age")) %>%
              spread(source, N) %>%
              {if(is.null(simOut)){
                mutate(.,
                       fit025 = exp(log(fit) - 1.96 * sdLog),
                       fit05  = exp(log(fit) - 1.64 * sdLog),
                       fit25  = exp(log(fit) - 0.674 * sdLog),
                       fit75  = exp(log(fit) + 0.674 * sdLog),
                       fit95  = exp(log(fit) + 1.64 * sdLog),
                       fit975 = exp(log(fit) + 1.96 * sdLog))
              } else .} %>%
              mutate(age = as.numeric(age),
                     variable = varName))
  }
  return(tsPl)
}