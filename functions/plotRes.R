plotRes <- function(res, simOut = NULL, noScaledYearsFit){
  d2plot <- 
    res %>%
    group_by(model, scenario, year, fleetName, age) %>%
    summarise(meanObs = mean(observation),
              meanPred = mean(mean),
              meanRes = mean(residual),
              meanAbsRes = mean(abs(residual)),
              nObs = length(residual),
              meanRes95lo = meanRes - 1.96 * sd(residual)/sqrt(nObs),
              meanRes95hi = meanRes + 1.96 * sd(residual/sqrt(nObs)))
  
  
  colors2use <- RColorBrewer::brewer.pal(3, "Dark2")
  
  for (i in 1:length(d2plot$scenario %>% unique)) { 
    p <-
      ggplot(d2plot %>% filter(scenario == unique(d2plot$scenario)[i]),
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = meanPred)) +
      geom_point(aes(y = meanObs), color = "black", size = 1) +
      #geom_ribbon(aes(ymin = meanRes95lo, ymax = meanRes95hi), color = NA, alpha = 0.3) +
      geom_vline(xintercept = max(d2plot$year) - noScaledYearsFit + 1) +
      #geom_hline(yintercept = 0) +
      facet_grid(fleetName ~ age, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "predicted and observed"))
    
    print(p)
    
    # ggsave(paste0("./figures/meanPredVsObs_", unique(d2plot$scenario)[i], ".pdf"), 
    #        width = 12, height = 7)
    
    p <-
      ggplot(d2plot %>% filter(scenario == unique(d2plot$scenario)[i],
                               fleetName == "Residual catch",
                               model == "with misreporting"),
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = meanRes)) +
      geom_ribbon(aes(ymin = meanRes95lo, ymax = meanRes95hi), color = NA, alpha = 0.3) +
      geom_vline(xintercept = max(d2plot$year) - noScaledYearsFit + 1) +
      geom_hline(yintercept = 0) +
      facet_grid(fleetName ~ age, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "mean residual"))
    
    print(p)
    
    # ggsave(paste0("./figures/meanResiduals_", unique(d2plot$scenario)[i], ".pdf"), 
    #        width = 12, height = 7)
    
    p <-
      ggplot(d2plot %>% filter(scenario == unique(d2plot$scenario)[i],
                               year > 1982),
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = meanAbsRes)) +
      #geom_ribbon(aes(ymin = meanRes95lo, ymax = meanRes95hi), color = NA, alpha = 0.3) +
      geom_vline(xintercept = max(d2plot$year) - noScaledYearsFit + 1) +
      geom_hline(yintercept = 0) +
      facet_grid(fleetName ~ age, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "mean absolute residual"))
    
    print(p)
    
    # ggsave(paste0("./figures/meanAbsResiduals_", unique(d2plot$scenario)[i], ".pdf"), 
    #        width = 12, height = 7)
    
    p <-
      ggplot(res %>%
               group_by(year, model, scenario, fleetName) %>%
               #, age) %>%
               summarise(meanRes = mean(residual),
                         meanAbsRes = mean(abs(residual)),
                         nObs = length(residual),
                         meanRes95lo = meanRes - 1.96 * sd(residual)/sqrt(nObs),
                         meanRes95hi = meanRes + 1.96 * sd(residual/sqrt(nObs)),
                         meanAbsRes95lo = meanAbsRes - 1.96 * sd(abs(residual))/sqrt(nObs),
                         meanAbsRes95hi = meanAbsRes + 1.96 * sd(abs(residual))/sqrt(nObs)),
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = meanAbsRes)) +
      #geom_ribbon(aes(ymin = meanAbsRes95lo, ymax = meanAbsRes95hi), color = NA, alpha = 0.3) +
      geom_vline(xintercept = max(d2plot$year) - noScaledYearsFit + 1) +
      geom_hline(yintercept = 0) +
      facet_wrap(~fleetName, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "mean absolute residual"))
    
    print(p)
    
    p <-
      ggplot(res %>%
               group_by(model, scenario, fleetName) %>%
                        #, age) %>%
               summarise(meanRes = mean(residual),
                         meanAbsRes = mean(abs(residual)),
                         nObs = length(residual),
                         meanRes95lo = meanRes - 1.96 * sd(residual)/sqrt(nObs),
                         meanRes95hi = meanRes + 1.96 * sd(residual/sqrt(nObs)),
                         meanAbsRes95lo = meanAbsRes - 1.96 * sd(abs(residual))/sqrt(nObs),
                         meanAbsRes95hi = meanAbsRes + 1.96 * sd(abs(residual))/sqrt(nObs)),
             aes(color = model)) +
      geom_point(aes(x = model, y = meanAbsRes)) +
      geom_errorbar(aes(x = model, ymin = meanAbsRes95lo, ymax = meanAbsRes95hi)) +
      geom_hline(yintercept = 0) +
      facet_wrap(~fleetName, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "mean absolute residual"))
    
    print(p)
    
    # Plot cumulative absolute residual over time
    p <-
      ggplot(res %>%
               select(model, scenario, fleetName, age, year, residual) %>%
               group_by(model, scenario, fleetName, year) %>%
               mutate(abs_residual = abs(residual)) %>%
               summarise(abs_residual = sum(abs_residual)) %>% # sum over ages
               mutate(cumsum_abs_residual = cumsum(abs_residual)) %>%
               arrange(model, scenario, fleetName, year),
             aes(x = year, color = model, fill = model)) +
      geom_line(aes(y = cumsum_abs_residual)) +
      geom_vline(xintercept = max(d2plot$year) - noScaledYearsFit + 1) +
      facet_wrap(~fleetName, scales = "free") +
      theme_bw() +
      scale_color_manual(values = colors2use[c(1,3)]) +
      scale_fill_manual(values = colors2use[c(1,3)]) +
      ylab("") +
      ggtitle(paste(unique(d2plot$scenario)[i], "cumulative absolute residual"))
    
    print(p)
  }
}