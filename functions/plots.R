plots <- function(fit, simOut, err) {
  # Plot an example random effects at age
  plotReTsAtAge(fit[[300]], simOut[[300]])
  
  # Plot time series error
  plotTsError(err, 
              scaled_yearsFit = fit[[1]]$conf$keyScaledYears, 
              scaled_yearsSim = simOut[[1]]$scaled_yearsSim)
  
  # Decile plots to indicate how well-calibrated the CIs are
  plotDecile(err)
  
  # Probability of stating misreporting vs abs(1 - true scale value)
  plotProbPredictMis(err)
  
}