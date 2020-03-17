plotReTsAtAge <- function(fit, simOut = NULL) {
  
  if(!is.null(simOut)) dat2plotTrue <- extractTsPl(fit, simOut) %>% rename(tru = fit)
  
  dat2plotFit <- extractTsPl(fit)
  
  dat2plot <- 
    dat2plotFit %>%
    {if(!is.null(simOut)) left_join(., dat2plotTrue) else .} %>%
    gather(model, value, -year, -age, -sdLog, -variable, 
           -fit025, -fit05, -fit25, -fit75, -fit95, -fit975) %>%
    mutate(age = paste("age", age))
  
  
  # Plot all variables together
  p <-
    ggplot(dat2plot, aes(x = year)) +
    geom_line(aes(y = value, color = model)) +
    geom_ribbon(aes(ymax = fit975, ymin = fit025), 
                color = NA, alpha = 0.3) +
    facet_grid(variable~age, scales = "free_y") +
    theme_bw() +
    scale_color_manual(values = c("black", "red")) +
    scale_fill_manual(values = c("black", "red"))+
    scale_x_continuous(breaks = seq(min(dat2plot$year), 
                                    max(dat2plot$year), by = 10)) +
    xlab("Year")
  
  plot(p)
  
  # Plot just the scale parameter
  p <-
    ggplot(dat2plot %>% filter(variable == "S"),
           aes(x = year)) +
      geom_line(aes(y = value, color = model)) +
      geom_point(aes(y = value, color = model)) +
      geom_hline(yintercept = 1,
                 lty = 2) +
      geom_ribbon(aes(ymax = fit975, ymin = fit025), 
                  color = NA, alpha = 0.3) +
      facet_wrap(~age, nrow = 1) +
      theme_bw() +
      scale_color_manual(values = c("black", "red"))+
      theme(axis.text.x = element_text(size = 7)) +
      ylab("Scale parameter estimate") +
      xlab("Year") +
      if(!is.null(simOut)) {
       geom_vline(xintercept = min(simOut$trueParams$conf$keyScaledYears))
      }
  plot(p)
}