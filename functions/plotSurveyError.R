plotSurveyError <- function(err, scaled_yearsSim = NULL, LOerr) {

  colors2use <- RColorBrewer::brewer.pal(4, "Dark2")
  
  scaled_yearsSimStart <- data.frame(scenario = c("random walk scenario", 
                                                  "random walk 10yrs scenario"),
                                     start = c(1995, 2005))
  
  # Plot mean error in leave out survey observation vs year
  err2plot <-
    err %>%
    dplyr::filter(variable %in% c("Survey")) %>%
    dplyr::select(model, scenario, year, variable, age, 
                  replicate, error, error_pc, abs_error_pc) %>%
    dplyr::group_by(model, scenario, age, year, variable) %>%
    dplyr::summarise(error_pc_median = median(error_pc, na.rm = T),
                     error_pc_hi = quantile(error_pc, 0.75, na.rm = T),
                     error_pc_lo = quantile(error_pc, 0.25, na.rm = T),
                     mape  = median(abs_error_pc, na.rm = T),
                     nObs = length(abs_error_pc),
                     mape_hi   = quantile(abs_error_pc, 0.75, na.rm = T),
                     mape_lo   = quantile(abs_error_pc, 0.25, na.rm = T)) %>%
    mutate(age = paste0("age-", age))
  
  # Significance test of difference in errors (Mann-Whitney U Test) #<<<<<<<<<<<<<<<<<
  
  # MAPE
  p <-
    ggplot(err2plot,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = mape)) +
    geom_ribbon(aes(ymin = mape_lo, ymax = mape_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    facet_grid(age~scenario, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Median absolute percent error") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    if (LOerr) {
      ggtitle("Survey estimation error in the leave-out year")
    } else {
      ggtitle("Survey estimation error")
    }
  
    p <-
      p +
      theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
      geom_vline(data = scaled_yearsSimStart, aes(xintercept = start))  
    
  
  print(p)
  
  # Median percent error
  p <-
    ggplot(err2plot,
           aes(x = year, color = model, fill = model)) +
    geom_line(aes(y = error_pc_median)) +
    geom_ribbon(aes(ymin = error_pc_lo, ymax = error_pc_hi), color = NA, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    facet_grid(age~scenario, scales = "free_y") +
    theme_bw() +
    xlab("Year") +
    ylab("Median percent error (fit - true)") +
    scale_color_manual(values = colors2use) +
    scale_fill_manual(values = colors2use) +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
    if (LOerr) {
      ggtitle("Survey estimation error in the leave-out year")
    } else {
      ggtitle("Survey estimation error")
    }
    
  p <-
    p +
      geom_vline(data = scaled_yearsSimStart, aes(xintercept = start))
  
  print(p)
  
  
  # t-test p-values
  err2test <- err %>% 
      dplyr::filter(variable %in% c("Survey")) %>%
      select(scenario, replicate, model, fleet, year, age, abs_error_pc) %>%
      spread(model, abs_error_pc) %>%
      group_by(scenario, replicate, age) %>%
      summarise(pval = t.test(base_LO, with_misreporting_LO, paired = TRUE)$p.value,
                est  = t.test(base_LO, with_misreporting_LO, paired = TRUE)$estimate)
  
}