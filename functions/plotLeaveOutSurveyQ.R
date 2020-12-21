plotLeaveOutSurveyQ <- function(scaled_yearsSim, fitLO, fit) {
  

  # Plot true vs estimated catchability parameters
  
  # Extract catchability estimates
  surveyQs_No <- data.frame()
  surveyQs_No_full <- data.frame()
  for(h in 1:length(fitLO)) {
    for(i in 1:length(fitLO[[h]]$fits)) {
      fit2extract <- fitLO[[h]]$fits[[i]]
      
      if (class(fit2extract) != "try-error") {
        
        dat <- data.frame(scenario = sim_labelAccept$scenario[h],
                          rep = sim_labelAccept$replicate[h],
                          leaveOutYears = fitLO[[h]]$leaveOutYears[[i]], 
                          qtable(fit2extract)[,],
                          survey = substr(row.names(qtable(fit2extract)[,]), 1, 11))
        
        surveyQs_No <- rbind(surveyQs_No, dat)
      }
      
    }
    # Full dataset fit
    fit2extract_full <- fit[[h]]
    
    if (class(fit2extract_full) != "try-error") {
      
      dat_full <- data.frame(scenario = sim_labelAccept$scenario[h],
                             rep = sim_labelAccept$replicate[h],
                             qtable(fit2extract_full)[,],
                             survey = substr(row.names(qtable(fit2extract_full)[,]), 1, 11))
      
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
    model <- ifelse(is.numeric(fit[[1]]$pl$logS), "With misreporting", "Base")
      
    p <-
      ggplot(data = surveyQs_No2plot %>% filter(scenario == scenario2plot),
             aes(x = leaveOutYears, y = Q_mean)) +
      geom_line(aes(color = "loo dataset")) +
      geom_ribbon(aes(ymin = Q_lo, ymax = Q_hi), fill = "red", alpha = 0.3) +
      geom_line(aes(y = Q_mean_full, color = "full dataset")) +
      geom_ribbon(aes(ymin = Q_lo_full, ymax = Q_hi_full), fill = "black", alpha = 0.3) +
      facet_grid(age ~ survey) +
      theme_bw() +
      theme(axis.title   = element_text(size = 14),
            plot.title   = element_text(size = 16),
            strip.text   = element_text(size = 9)) +
      xlab("Leave-out year") +
      ylab("Q estimate") +
      scale_color_manual(values = c("black", "red")) +
      ggtitle(paste0(model, " model Q estimate in leave-out year (", scenario2plot, " scenario)")) +
      if (scenario2plot != "no misreporting") {
        geom_vline(aes(xintercept = min(scaled_yearsSim)))
      }
    plot(p)
  }
}











