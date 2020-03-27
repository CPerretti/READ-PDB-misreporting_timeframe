calcErr <- function(fit, simOut){
  # Random effects error
  errRe   <- map2_dfr(fit, simOut, calcReTsError)
  # Catch and SSB error
  errCSSB <- map2_dfr(fit, simOut, calcCSSBError)
  
  err0 <- rbind(errRe, errCSSB)
  
  err <- 
    err0 %>%
    left_join({err0 %>%
               filter(variable == "S") %>%
               mutate(predictMisreporting95 = ifelse((exp(log(fit) - 1.96 * sdLog) < 1)  &  
                                            (exp(log(fit) + 1.96 * sdLog) > 1), 
                                          "No misreporting", "Yes misreporting"),
           trueMisreporting = ifelse(tru == 1, "No misreporting", "Yes misreporting"))}) %>%
    mutate(scenario = ifelse(scenario == "rw", "random walk", scenario),
           scenario = paste(scenario, "scenario"),
           scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                   "fixed scenario",
                                                   "random walk scenario",
                                                   "uniform random scenario")))
  
  return(err)
}