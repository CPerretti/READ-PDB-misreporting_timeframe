## Plot timeseries error ##################################
plotTsError <- function(err, LOerr, plotScale) {
  
  colors2use <- RColorBrewer::brewer.pal(4, "Dark2")
  
  scaled_yearsSimStart <- data.frame(scenario = c("random walk scenario", 
                                                  "random walk 10yrs scenario"),
                                     start = c(1995, 2005))
  
  # Plot error in important variables vs year
  err2plot_CSSB <-
    err %>%
    dplyr::filter(variable %in% c("catch", "ssb", "F", "N")) %>%
    dplyr::select(model, scenario, year, variable, age, replicate, error, error_pc, abs_error_pc) %>%
    dplyr::group_by(model, scenario, year, variable) %>%
    dplyr::summarise(error_pc_median = median(error_pc, na.rm = T),
                     mape  = median(abs_error_pc, na.rm = T),
                     nObs = length(abs_error_pc),
                     error_pc_hi = quantile(error_pc, 0.75, na.rm = T),
                     error_pc_lo = quantile(error_pc, 0.25, na.rm = T),
                     mape_hi   = quantile(abs_error_pc, 0.75, na.rm = T),
                     mape_lo   = quantile(abs_error_pc, 0.25, na.rm = T))
  
  # Median absolute percent error
  p <-
    ggplot(err2plot_CSSB,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = mape)) +
    geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    geom_vline(data = scaled_yearsSimStart, aes(xintercept = start)) +
    facet_grid(scenario~variable, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Median absolute percent error") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
    if (LOerr) {
      ggtitle("Estimation error in leave-out year")
    } else {
      ggtitle("Estimation error")
    }
    
  
  print(p)
  
  # Median percent error
  p <-
    ggplot(err2plot_CSSB,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = error_pc_median)) +
    geom_ribbon(aes(ymin = error_pc_lo, ymax = error_pc_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    geom_vline(data = scaled_yearsSimStart, aes(xintercept = start)) +
    facet_grid(scenario~variable, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Median percent error (fit - true)") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
    if (LOerr) {
      ggtitle("Estimation error in leave-out year")
    } else {
      ggtitle("Estimation error")
    }
  
  print(p)
  
  if (plotScale) {
    # Plot error of Scale estimates
    if (any(err$variable == "S")) {
      
      # Plot average error for each scenario
      err2plot_Scale_overall <-
        err %>%  
        dplyr::filter(variable %in% c("S")) %>%
        dplyr::select(model, scenario, year, variable, age, replicate, 
                      error_pc, abs_error_pc, abs_error) %>%
        dplyr::group_by(model, scenario, year, variable) %>%
        dplyr::summarise(error_pc_median = median(error_pc, na.rm = T),
                         mape  = median(abs_error_pc, na.rm = T),
                         nObs = length(abs_error_pc),
                         error_pc_hi = quantile(error_pc, 0.75, na.rm = T),
                         error_pc_lo = quantile(error_pc, 0.25, na.rm = T),
                         mape_hi   = quantile(abs_error_pc, 0.75, na.rm = T),
                         mape_lo   = quantile(abs_error_pc, 0.25, na.rm = T))
      
      p <-
        ggplot(err2plot_Scale_overall,
               aes(x = year, color = model, fill = model)) +
        geom_line(aes(y = mape)) +
        geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
        geom_hline(yintercept = 0) +
        geom_vline(data = scaled_yearsSimStart, aes(xintercept = start)) +
        facet_grid(~scenario) +
        theme_bw() +
        xlab("Year") +
        ylab("Median absolute percent error") +
        scale_color_manual(values = colors2use[3]) +
        scale_fill_manual(values = colors2use[3]) +
        if (LOerr) {
          ggtitle("Scale estimation error in leave-out year")
        } else {
          ggtitle("Scale estimation error")
        }
      
      print(p)
      
      
      # Plot median percent error
      p <-
        ggplot(err2plot_Scale_overall,
               aes(x = year, color = model, fill = model
               )) +
        geom_line(aes(y = error_pc_median)) +
        geom_ribbon(aes(ymin = error_pc_lo, ymax = error_pc_hi), color = NA, alpha = 0.3) +
        geom_hline(yintercept = 0) +
        geom_vline(data = scaled_yearsSimStart, aes(xintercept = start)) +
        facet_grid(~scenario) +
        theme_bw() +
        xlab("Year") +
        ylab("Median percent error (fit - true)") +
        scale_color_manual(values = colors2use[3]) +
        scale_fill_manual(values = colors2use[3]) +
        if (LOerr) {
          ggtitle("Scale estimation error in leave-out year")
        } else {
          ggtitle("Scale estimation error")
        }
      
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
          scale_color_manual(values = c("black", colors2use[3])) +
          scale_fill_manual(values = c(colors2use[3]), guide = "none") +
          theme(legend.title = element_blank()) +
          if (LOerr) {
            ggtitle(paste0("Scale parameter estimates in the leave-out year
                  (", scenarios2plot[i], ")"))
          } else {
            ggtitle(paste0("Scale parameter estimates (", scenarios2plot[i], ")"))
          }
        
        print(p)
      }
      
    }  
  }
  
  
}