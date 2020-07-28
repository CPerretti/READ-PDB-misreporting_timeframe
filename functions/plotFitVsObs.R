plotFitVsObs <- function(fitVsObs, simOut = NULL){
  
  
  colors2use <- RColorBrewer::brewer.pal(3, "Dark2")
  
    p <-
      ggplot(fitVsObs,
             aes(x = year, color = model)) +
      geom_line(aes(y = logobsFit)) +
      geom_point(aes(y = logobs), color = "black", size = 1) +
      facet_wrap(fleetName ~ age, scales = "free", nrow = 3) +
      theme_bw() +
      scale_fill_manual(values = colors2use[1:3]) +
      ylab("")
    
    print(p)
}
