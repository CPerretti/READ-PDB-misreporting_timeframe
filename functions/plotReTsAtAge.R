plotReTsAtAge <- function(fit) {
  dat2plot <- extractTsPl(fit)
  
  
  # Plot all variables together
  p <-
    ggplot(dat2plot, aes(x = year)) +
    geom_line(aes(y = fit)) +
    geom_hline(yintercept = 1, color = "black",
               lty = 2) +
    geom_ribbon(aes(ymin = fit75, ymax = fit25), 
                alpha = 0.3, color = NA) +
    facet_grid(variable~age, scales = "free_y") +
    theme_bw() +
    scale_x_continuous(breaks = seq(min(dat2plot$year), 
                                    max(dat2plot$year), by = 10))
  
  plot(p)
  
  # Plot just the scale parameter
  dat2plotS <- dat2plot %>% filter(variable == "S")
  p <-
    ggplot(dat2plotS, aes(x = year)) +
      geom_line(aes(y = fit)) +
      geom_hline(yintercept = 1, color = "black",
                 lty = 2) +
      geom_ribbon(aes(ymin = fit75, ymax = fit25), 
                  alpha = 0.3, color = NA) +
      facet_wrap(~age, nrow = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 7)) +
      ylab("Scale parameter estimate")
  plot(p)
}