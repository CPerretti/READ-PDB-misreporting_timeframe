plotHistFracMixS <- function(fit, simOut) {
  dfFracMixS <-
    map2_dfr(fit, simOut,
             function(fit, simOut){
               data.frame(scenario = simOut$sim_label$scenario,
                          replicate = simOut$sim_label$replicate,
                          logitFracMixS = fit$sdrep$par.fixed["logitFracMixS"])}) %>%
    mutate(fracMixS = 1/(1 + exp(-logitFracMixS)),
           scenario = ifelse(scenario == "rw", "random walk", scenario),
           scenario = paste(scenario, "scenario"),
           scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                   "fixed scenario",
                                                   "random walk scenario",
                                                   "uniform random scenario")))
  
  p <- 
    ggplot(dfFracMixS, aes(x = fracMixS)) +
    geom_histogram() +
    facet_wrap(~scenario) +
    theme_bw()
  
  print(p)
}