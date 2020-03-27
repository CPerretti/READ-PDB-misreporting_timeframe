plotDecile <- function(err) {
  # Decile plots to indicate how well-calibrated the CIs are
  p <-
    ggplot(err %>%
             filter(variable == "S") %>%
             group_by(scenario, variable, decile) %>%
             summarise(decileCount = n()) %>%
             left_join({err %>% 
                 group_by(scenario, variable) %>% 
                 summarise(totalCount = n())}) %>%
             mutate(decilePc = 100 * decileCount / totalCount,
                    decile = factor(decile, levels = c(1:10))),
           aes(x = decile, y = decilePc)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 10) +
    facet_wrap(~scenario) +
    theme_bw() +
    xlab("Decile") +
    ylab("Percent of observations") +
    ggtitle("Scale parameter confidence interval performance")
  
  print(p)
}