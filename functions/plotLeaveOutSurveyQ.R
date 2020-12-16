
scaled_yearsSim = simOutAccept[[1]]$scaled_yearsSim

# Plot true vs estimated catchability parameters

# Extract catchability estimates
surveyQs_No <- data.frame()
surveyQs_No_full <- data.frame()
for(h in 1:length(fitNoSimLOAccept)) {
  for(i in 1:length(fitNoSimLOAccept[[h]]$fits)) {
    fitNo2extract <- fitNoSimLOAccept[[h]]$fits[[i]]
    
    
    if (class(fitNo2extract) != "try-error") {
      
      dat <- data.frame(scenario = sim_labelAccept$scenario[h],
                        rep = sim_labelAccept$replicate[h],
                        leaveOutYears = fitNoSimLOAccept[[h]]$leaveOutYears[[i]], 
                        qtable(fitNo2extract)[,],
                        survey = substr(row.names(qtable(fitNo2extract)[,]), 1, 11))
      
      surveyQs_No <- rbind(surveyQs_No, dat)
    }
    
  }
  # Full dataset fit
  fitNo2extract_full <- fitNoSimAccept[[h]]
  
  if (class(fitNo2extract_full) != "try-error") {
    
    dat_full <- data.frame(scenario = sim_labelAccept$scenario[h],
                           rep = sim_labelAccept$replicate[h],
                           qtable(fitNo2extract_full)[,],
                           survey = substr(row.names(qtable(fitNo2extract_full)[,]), 1, 11))
    
    surveyQs_No_full <- rbind(surveyQs_No_full, dat_full)
  }
  
}


surveyQs_No2plot <-
  surveyQs_No %>%
  gather(age, logQ, -scenario, -rep, -leaveOutYears, -survey) %>%
  mutate(age = paste0("age-", substr(age, 2,2)),
         Q = exp(logQ)) %>%
  group_by(scenario, leaveOutYears, survey, age) %>%
  summarise(Q_mean = mean(Q),
            Q_hi  = quantile(Q, 0.75, na.rm = T),
            Q_lo  = quantile(Q, 0.25, na.rm = T)) %>%
  left_join({surveyQs_No_full %>%
             gather(age, logQ, -scenario, -rep, -survey) %>%
             mutate(age = paste0("age-", substr(age, 2,2)),
                    Q = exp(logQ)) %>%
             group_by(scenario, survey, age) %>%
             summarise(Q_mean_full = mean(Q),
                       Q_hi_full  = quantile(Q, 0.75, na.rm = T),
                       Q_lo_full  = quantile(Q, 0.25, na.rm = T))})
  


for (i in 1:length(unique(surveyQs_No2plot$scenario))) {
  
  scenario2plot <- unique(surveyQs_No2plot$scenario)[i]
  
  p <-
    ggplot(data = surveyQs_No2plot %>% filter(scenario == scenario2plot),
         aes(x = leaveOutYears, y = Q_mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = Q_lo, ymax = Q_hi), alpha = 0.3) +
    geom_line(aes(y = Q_mean_full), color = "red") +
    geom_ribbon(aes(ymin = Q_lo_full, ymax = Q_hi_full), fill = "red", alpha = 0.3) +
    facet_grid(age ~ survey) +
    theme_bw() +
    theme(axis.title   = element_text(size = 14),
          plot.title   = element_text(size = 16),
          strip.text   = element_text(size = 9)) +
    xlab("Leave-out year") +
    ylab("Q estimate") +
    ggtitle(paste0(scenario2plot, " scenario Q estimate in leave-out year")) +
    if (scenario2plot != "no misreporting") {
      geom_vline(aes(xintercept = min(scaled_yearsSim)))
    }
  plot(p)
}









