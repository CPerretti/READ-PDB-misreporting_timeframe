plots <- function(fit, simOut, err) {
  # Plot an example random effects at age
  plotReTsAtAge(fit[[1]], simOut[[1]])
  
  # Plot time series error
  plotTsError(err, 
              scaled_yearsFit = fit[[1]]$conf$keyScaledYears, 
              scaled_yearsSim = simOut[[1]]$scaled_yearsSim)
  
  # Plot mean fit vs mean true for each scenario
  plotMeanFitVTru(err, simOut[[1]])
  
  # Decile plots to indicate how well-calibrated the CIs are
  plotDecile(err)
  
  # Probability of stating misreporting vs abs(1 - true scale value)
  plotProbPredictMis(err)
  
  # Plot timeseries of proportion of runs where misreporting was concluded
  plotPropMis(err, simOut[[1]])
  
  # Plot mean or median scale parameter value for each scenario
  plotMeanScale(err, simOut[[1]])
  
  # Create histogram of fracMixS for each scenario
  dfFracMixS <-
    map2_dfr(fitMisSimAccept, simOutAccept,
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

  ggplot(dfFracMixS, aes(x = fracMixS)) +
    geom_histogram() +
    facet_wrap(~scenario) +
    theme_bw()
  
}
  
  