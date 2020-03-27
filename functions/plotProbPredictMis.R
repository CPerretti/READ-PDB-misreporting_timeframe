plotProbPredictMis <- function(err) {
  x <-
    err %>%
    filter(variable == "S") %>%
    select(scenario, replicate, year, age, tru, predictMisreporting95) %>%
    mutate(predictMisreporting95 = ifelse(predictMisreporting95 == "No misreporting", 0, 1),
           # Percent misreported is 100 * (True catch - Reported catch) / True catch,
           # and Scale is True catch / Reported catch, so...
           percentMisreported = abs(100 * (1 - 1/tru))) 
  #plotProbMis()
  
  fit <- 
    glm(predictMisreporting95 ~ percentMisreported, 
        data = x,
        family = binomial)
  
  ## Calculate x50's
  x50_ci <- MASS::dose.p(fit, p = 0.5)
  x50s <- data.frame(x50 = x50_ci[1],
                     pc025 = x50_ci[1] - 1.96 * attributes(x50_ci)$SE,
                     pc975 = x50_ci[1] + 1.96 * attributes(x50_ci)$SE)
  
  ## Calculate ogive using all years w.r.t. age
  ilink <- family(fit)$linkinv
  var2use <- attr(fit$terms, "term.labels")
  dependentVar <- min(fit$data[[var2use]]):max(fit$data[[var2use]])
  
  ogive <-
    predict(fit,
            newdata = tibble(!!var2use := dependentVar),
            type = "link",
            se.fit = TRUE) %>%
    as.data.frame() %>%
    mutate(pred = ilink(fit), # note this "fit" is output from predict
           pc025  = ilink(fit - 1.96 * se.fit),
           pc975  = ilink(fit + 1.96 * se.fit),
           !!var2use := dependentVar) %>%
    select(pred, !!var2use, pc025, pc975)
  
  p <-
    ggplot(ogive) +
    geom_rect(data = x50s,
              aes(xmin = pc025, xmax = pc975, ymin = 0, ymax = 1),
              alpha = 0.2, fill = "red") +
    geom_line(aes(x = percentMisreported, y = pred, color = "Probabily of predicting misreporting")) +
    geom_ribbon(aes(x = percentMisreported, ymax = pc975, ymin = pc025), alpha = 0.4) +
    geom_segment(data = x50s,
                aes(x = x50, y=0, xend = x50, yend = 1, color = "50% probability")) +
    theme_bw() +
    scale_color_manual(values = c("red", 
                                  "black")) +
    #scale_fill_manual(values = "blue") +
    ylab("Probability of predicting misreporting") +
    xlab("Percent misreported") +
    labs(caption = "Percent misreported = abs((True catch - Reported catch)/True catch) * 100") +
    theme(legend.title = element_blank(),
          legend.position = "none",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          plot.caption.position =  "panel")
  
  print(p)
}