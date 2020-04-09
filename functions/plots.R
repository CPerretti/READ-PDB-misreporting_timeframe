plots <- function(fit, simOut, err) {
  # Plot an example random effects at age
  plotReTsAtAge(fit[[1]], simOut[[1]])
  
  # Plot time series error
  plotTsError(err, 
              scaled_yearsFit = fit[[1]]$conf$keyScaledYears, 
              scaled_yearsSim = simOut[[1]]$scaled_yearsSim)
  
  # Plot mean fit vs mean true for each scenario
  colors2use <- RColorBrewer::brewer.pal(3, "Dark2")
  
  scaled_yearsSim = simOut[[1]]$scaled_yearsSim
  
  d2plot <- 
    err %>%
    filter(variable == "S") %>%
    group_by(scenario, year, age, model) %>%
    summarise(fit_mean = mean(fit),
              fit_975 = mean(fit) + 1.96 * sd(fit),
              fit_025 = mean(fit) - 1.96 * sd(fit),
              tru_mean = mean(tru))
    
  p <-
    ggplot(d2plot,
         aes(x = year)) +
    geom_line(aes(y = fit_mean, color = model)) +
    geom_point(aes(y = fit_mean, color = model)) +
    geom_line(aes(y = tru_mean, color = "true")) +
    geom_point(aes(y = tru_mean, color = "true")) +
    geom_hline(yintercept = 1, color = "dark grey") +
    geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                              "fixed scenario", 
                                              "random walk scenario",
                                              "uniform random scenario"),
                                 xint = c(NA, rep(min(scaled_yearsSim), 3))),
               aes(xintercept = xint)) +
    facet_grid(scenario~age, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Mean scale parameter value") +
    scale_color_manual(values = c(colors2use[3], "black")) +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 12),
          legend.title = element_blank())
  
  print(p)
  
  # Decile plots to indicate how well-calibrated the CIs are
  plotDecile(err)
  
  # Probability of stating misreporting vs abs(1 - true scale value)
  plotProbPredictMis(err)
  
  # Plot timeseries of proportion of runs where misreporting was concluded
  plotPropMis(err, simOut[[1]])
  
  # Plot mean or median scale parameter value for each scenario
  plotMeanScale(err, simOut[[1]])
  
  
}