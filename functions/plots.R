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
  plotHistFracMixS(fitMisSimAccept, simOutAccept)
  
}
  
  