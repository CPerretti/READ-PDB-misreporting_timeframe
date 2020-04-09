plotMeanScale <- function(err, simOut) {
  d2plot <-
    err %>%
    filter(variable == "S") %>%
    group_by(scenario, year) %>%
    summarise(meanScale = mean(tru),
              medianScale = median(tru),
              sd = sd(tru),
              var = sd^2,
              iqr = IQR(tru),
              range = max(tru) - min(tru))
  
  p <- 
    ggplot(d2plot, aes(x = year, y = range)) +
    geom_line() +
    #geom_ribbon(aes(ymin = meanScale - 1.96*sd, ymax = meanScale + 1.96*sd),
    #           color = NA, alpha = 0.3) +
    geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                              "fixed scenario", 
                                              "random walk scenario",
                                              "uniform random scenario"),
                                 xint = c(NA, rep(min(simOut$scaled_yearsSim), 3))),
               aes(xintercept = xint)) +
    facet_wrap(~scenario) +
    theme_bw() +
    xlab("Year") +
    ylab("Scale parameter range")
  
  print(p)
}