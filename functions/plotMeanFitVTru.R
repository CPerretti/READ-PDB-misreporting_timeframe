plotMeanFitVTru <- function(err, simOut){
  colors2use <- RColorBrewer::brewer.pal(4, "Dark2")
  
  scaled_yearsSim = simOut$scaled_yearsSim
  
  d2plot <- 
    err %>%
    #filter(variable == "S") %>%
    group_by(variable, scenario, year, age, model) %>%
    summarise(fit_mean = mean(fit),
              fit_median = median(fit),
              fit_975 = mean(fit) + 1.96 * sd(fit),
              fit_025 = mean(fit) - 1.96 * sd(fit),
              tru_mean = mean(tru),
              tru_median = median(tru))
  
  for (i in 1:length(unique(d2plot$variable))) {
    d2plot2 <- d2plot %>% filter(variable == unique(d2plot$variable)[i])
    p <-
      ggplot(d2plot2,
             aes(x = year)) +
      geom_line(aes(y = fit_median, color = model)) +
      geom_point(aes(y = fit_median, color = model)) +
      geom_line(aes(y = tru_median, color = "true")) +
      geom_point(aes(y = tru_median, color = "true")) +
      geom_hline(yintercept = 1, color = "dark grey") +
      geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                                "fixed scenario", 
                                                "random walk scenario",
                                                "uniform random scenario"),
                                   xint = c(NA, rep(min(scaled_yearsSim), 3))),
                 aes(xintercept = xint)) +
      facet_grid(age~scenario, scales = "free_y") +
      theme_bw() +
      xlab("Year") +
      ylab(paste("Median", unique(d2plot$variable)[i], "value")) +
      scale_color_manual(values = c(colors2use, "black")) +
      theme(axis.title   = element_text(size = 14),
            plot.title   = element_text(size = 16),
            strip.text   = element_text(size = 12),
            legend.title = element_blank()) +
      ggtitle(paste0("Median ", unique(d2plot$variable)[i], " estimated vs true"))
    
    p <-
      p +
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
  
}