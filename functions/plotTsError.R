## Plot timeseries error ##################################
plotTsError <- function(err, scaled_yearsFit, scaled_yearsSim) {
  
  colors2use <- RColorBrewer::brewer.pal(4, "Dark2")
  
  
  # Plot mean error in catch and ssb vs year
  err2plot_CSSB <-
    err %>%
    dplyr::filter(variable %in% c("catch", "ssb", "F", "N")) %>%
    dplyr::select(model, scenario, year, variable, age, replicate, error, error_pc, abs_error_pc) %>%
    dplyr::group_by(model, scenario, year, variable) %>%
    dplyr::summarise(error_pc_mean = mean(error, na.rm = T),
                     mape  = mean(abs_error_pc, na.rm = T),
                     nObs = length(abs_error_pc),
                     error_pc_hi   = error_pc_mean + 1.96 * sd(error, na.rm = T)/sqrt(nObs),
                     error_pc_lo  = error_pc_mean - 1.96 * sd(error, na.rm = T)/sqrt(nObs),
                     mape_hi   = mape + 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs),
                     mape_lo   = mape - 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs))
  p <-
    ggplot(err2plot_CSSB,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = mape)) +
    geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    facet_grid(scenario~variable, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Mean absolute percent error") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    ggtitle("Estimation error in leave-out year")  +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9))
  
  print(p)
  
  # Plot error of Scale estimates
  if (any(err$variable == "S")) {
    
    # Plot average error for each scenario
    err2plot_Scale_overall <-
      err %>%  
      dplyr::filter(variable %in% c("S")) %>%
      dplyr::select(model, scenario, year, variable, age, replicate, 
                    error_pc, abs_error_pc, abs_error) %>%
      dplyr::group_by(model, scenario, year, variable) %>%
      dplyr::summarise(error_pc_mean = mean(error_pc, na.rm = T),
                       mape  = mean(abs_error_pc, na.rm = T),
                       mae   = mean(abs_error),
                       nObs  = length(abs_error_pc),
                       error_pc_hi = error_pc_mean + 1.96 * sd(error_pc, na.rm = T)/sqrt(nObs),
                       error_pc_lo = error_pc_mean - 1.96 * sd(error_pc, na.rm = T)/sqrt(nObs),
                       mape_hi   = mape + 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs),
                       mape_lo   = mape - 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs),
                       mae_hi   = mae + 1.96 * sd(mae, na.rm = T)/sqrt(nObs),
                       mae_lo   = mae - 1.96 * sd(mae, na.rm = T)/sqrt(nObs))
    
    p <-
      ggplot(err2plot_Scale_overall,
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = mape)) +
      geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
      geom_hline(yintercept = 0) +
      geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                                "fixed scenario", 
                                                "random walk scenario",
                                                "uniform random scenario"),
                                   xint = c(NA, rep(min(scaled_yearsSim), 3))),
                 aes(xintercept = xint)) +
      facet_grid(~scenario) +
      theme_bw() +
      xlab("Year") +
      ylab("Mean absolute percent error") +
      scale_color_manual(values = colors2use[2:3]) +
      scale_fill_manual(values = colors2use[2:3]) +
      ggtitle("Scale estimation error") 
    
    print(p)
    
    
    # Plot mean percent error
    p <-
      ggplot(err2plot_Scale_overall,
             aes(x = year, color = model#, 
                 #fill = model
                 )) +
      geom_line(aes(y = error_pc_mean)) +
      geom_hline(yintercept = 0) +
      geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                                "fixed scenario", 
                                                "random walk scenario",
                                                "uniform random scenario"),
                                   xint = c(NA, rep(min(scaled_yearsSim), 3))),
                 aes(xintercept = xint)) +
      facet_grid(~scenario) +
      theme_bw() +
      xlab("Year") +
      ylab("Mean percent error (fit - true)/true") +
      scale_color_manual(values = colors2use[2:3]) +
      #scale_fill_manual(values = colors2use[2:3]) +
      ggtitle("Scale estimation error") 
    
    print(p)
    
    
    # Plot example fit vs true scale
    err2plot_Scale <-
      err %>%
      dplyr::mutate(fit_975 = exp(log(fit) + 1.96 * sdLog),
                    fit_025 = exp(log(fit) - 1.96 * sdLog),
                    fit_95 = exp(log(fit)  + 1.645 * sdLog),
                    fit_05 = exp(log(fit)  - 1.645 * sdLog),
                    replicate = paste("replicate", replicate),
                    age = paste("age-", age)) %>%
      dplyr::filter(variable %in% c("S"))
    scenarios2plot <- unique(err2plot_Scale$scenario)
    for (i in 1:length(scenarios2plot)) {
      p <-
        ggplot(err2plot_Scale %>% 
                 dplyr::filter(scenario == scenarios2plot[i]) %>%
                 dplyr::filter(replicate %in% unique(replicate)[1:4]), 
               aes(x = year)) +
        geom_line(aes(y = fit, color = model)) +
        geom_line(aes(y = tru), color = "black") +
        geom_hline(yintercept = 1, color = "black",
                   lty = 2) +
        geom_line(aes(y = tru, color = "true")) +
        geom_ribbon(aes(ymin = fit_975, ymax = fit_025, fill = model), 
                    alpha = 0.3, color = NA) +
        facet_grid(replicate ~ age, scales = "free_y") +
        theme_bw() +
        xlab("Year") +
        ylab("Estimated and True scale parameter value") +
        scale_color_manual(values = c(colors2use[2:3], "black")) +
        scale_fill_manual(values = colors2use[2:3], guide = "none") +
        theme(legend.title = element_blank()) +
        ggtitle(paste0("Scale parameter estimates (", 
                       scenarios2plot[i], ")"))
      print(p)
    }
    
    # Example scale time series
    p <-
      ggplot(err2plot_Scale %>%
               dplyr::filter(replicate %in% unique(replicate)[1]), 
             aes(x = year)) +
      geom_line(aes(y = fit, color = model)) +
      geom_hline(yintercept = 1, color = "dark grey") +
      geom_line(aes(y = tru, color = "true")) +
      geom_ribbon(aes(ymin = fit_975, ymax = fit_025, fill = model), 
                  alpha = 0.3) +
      facet_grid(scenario~age, scales = "free_y") +
      theme_bw() +
      xlab("Year") +
      ylab("Scale parameter value") +
      #scale_y_continuous(breaks=seq(1,11,2)) +
      #scale_x_continuous(breaks = seq(2005, 2014, 4)) +
      scale_color_manual(values = c(colors2use[2:3], "black")) +
      scale_fill_manual(values = colors2use[2:3], guide = "none") +
      theme(axis.title   = element_text(size = 14),
            plot.title   = element_text(size = 16),
            strip.text   = element_text(size = 12),
            legend.title = element_blank())
    print(p)
    
    # ggsave(plot = p, "./figures/scale_examples.jpg", 
    #        width = 9, height = 10, dpi = 500)
    
  }
  
}