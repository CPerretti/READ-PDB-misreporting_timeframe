plotPropMis <- function(err, simOut) {
  
  d2plot <-
    err %>%
    filter(variable == "S") %>%
    group_by(scenario, year) %>%
    mutate(predictMisreporting95numeric = ifelse(predictMisreporting95 == "Yes misreporting", 1, 
                                                 ifelse(predictMisreporting95 == "No misreporting",
                                                        0, NA))) %>%
    summarise(propPositive = mean(predictMisreporting95numeric))
  
  p <-
    ggplot(d2plot, aes(x = year, y = propPositive)) +
    geom_point() +
    geom_line() +
    geom_vline(data = data.frame(scenario = c("no misreporting scenario", 
                                              "fixed scenario", 
                                              "random walk scenario",
                                              "uniform random scenario"),
                                 xint = c(NA, rep(min(simOut$scaled_yearsSim), 3))),
               aes(xintercept = xint)) +
    facet_wrap(~scenario) +
    theme_bw() +
    xlab("Year") +
    ylab("Probability of concluding misreporting is occurring")
  
  print(p)
}

