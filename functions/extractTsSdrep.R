extractTsSdrep <- function(fit) {
  model <- deparse(substitute(fit))
  data.frame(variable = names(fit$sdrep$value),
             value = fit$sdrep$value) %>%
    filter(variable %in% c("logssb", "logfbar", "logCatch", 
                           "logLand", "logR")) %>%
    mutate(variable = substr(variable, 4, 99),
           value    = exp(value),
           model    = model,
           year = rep(fit$data$years, length(unique(variable)))) %>%
    left_join(data.frame(variable = "Catch",
                         year = catchplot(fit, plot = FALSE)$obs$x,
                         obs  = catchplot(fit, plot = FALSE)$obs$y)) %>%
    left_join(data.frame(variable = c("Catch", "fbar", "Land", "R", "ssb"),
                         units    = c("(MT)", "", "(MT)", "(1000's)", "(MT)"))) %>%
    mutate(variableLabel = paste(variable, units))
}