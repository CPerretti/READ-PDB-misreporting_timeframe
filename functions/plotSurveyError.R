plotSurveyError <- function(err, scaled_yearsSim = NULL) {

  colors2use <- RColorBrewer::brewer.pal(4, "Dark2")
  
  # Plot mean error in catch and ssb vs year
  err2plot <-
    err %>%
    dplyr::filter(variable %in% c("Survey")) %>%
    dplyr::select(model, scenario, year, variable, age, 
                  replicate, error, error_pc, abs_error_pc) %>%
    dplyr::group_by(model, scenario, year, variable) %>%
    dplyr::summarise(error_pc_mean = mean(error, na.rm = T),
                     mape  = mean(abs_error_pc, na.rm = T),
                     nObs = length(abs_error_pc),
                     error_pc_hi   = error_pc_mean + 1.96 * sd(error, na.rm = T)/sqrt(nObs),
                     error_pc_lo  = error_pc_mean - 1.96 * sd(error, na.rm = T)/sqrt(nObs),
                     mape_hi   = mape + 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs),
                     mape_lo   = mape - 1.96 * sd(abs_error_pc, na.rm = T)/sqrt(nObs))
  p <-
    ggplot(err2plot,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = mape)) +
    geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    facet_wrap(~scenario, ncol = 1, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Mean absolute percent error") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    ggtitle("LOO CV error of survey observations in leave-out year")  +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
    if(!is.null(scaled_yearsSim)) {
      geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                                "fixed scenario", 
                                                "random walk scenario",
                                                "uniform random scenario"),
                                   xint = c(NA, rep(min(scaled_yearsSim), 3))),
                 aes(xintercept = xint))  
    }
    
  
  print(p)
}