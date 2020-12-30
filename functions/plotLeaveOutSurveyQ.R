plotLeaveOutSurveyQ <- function(sim_labelAccept, fitLONo, fitNo,
                                fitLOMis, fitMis) {
  
  scaled_yearsSimStart <- data.frame(scenario = c("random walk scenario", 
                                                  "random walk 10yrs scenario"),
                                     start = c(1995, 2005))
  
  # Plot true vs estimated catchability parameters
  
  # Extract catchability estimates
  surveyQs_No <- data.frame()
  surveyQs_No_full <- data.frame()
  surveyQs_Mis <- data.frame()
  surveyQs_Mis_full <- data.frame()  
  
  for(h in 1:length(fitLONo)) {
    for(i in 1:length(fitLONo[[h]]$fits)) {
      fitNo2extract <- fitLONo[[h]]$fits[[i]]
      fitMis2extract <- fitLOMis[[h]]$fits[[i]]
      
      if (class(fitNo2extract) != "try-error") {
        
        datNo <- data.frame(scenario = sim_labelAccept$scenario[h],
                          rep = sim_labelAccept$replicate[h],
                          leaveOutYears = fitLONo[[h]]$leaveOutYears[[i]], 
                          qtable(fitNo2extract)[,],
                          survey = substr(row.names(qtable(fitNo2extract)[,]), 1, 11))
        
        surveyQs_No <- rbind(surveyQs_No, datNo)
      }
      
      if (class(fitMis2extract) != "try-error") {
        
        datMis <- data.frame(scenario = sim_labelAccept$scenario[h],
                            rep = sim_labelAccept$replicate[h],
                            leaveOutYears = fitLOMis[[h]]$leaveOutYears[[i]], 
                            qtable(fitMis2extract)[,],
                            survey = substr(row.names(qtable(fitMis2extract)[,]), 1, 11))
        
        surveyQs_Mis <- rbind(surveyQs_Mis, datMis)
      }
      
    }
    # Full dataset fit
    fitNo2extract_full <- fitNo[[h]]
    fitMis2extract_full <- fitMis[[h]]
    
    if (class(fitNo2extract_full) != "try-error") {
      
      datNo_full <- data.frame(scenario = sim_labelAccept$scenario[h],
                             rep = sim_labelAccept$replicate[h],
                             qtable(fitNo2extract_full)[,],
                             survey = substr(row.names(qtable(fitNo2extract_full)[,]), 1, 11))
      
      surveyQs_No_full <- rbind(surveyQs_No_full, datNo_full)
    }
    
    if (class(fitMis2extract_full) != "try-error") {
      
      datMis_full <- data.frame(scenario = sim_labelAccept$scenario[h],
                               rep = sim_labelAccept$replicate[h],
                               qtable(fitMis2extract_full)[,],
                               survey = substr(row.names(qtable(fitMis2extract_full)[,]), 1, 11))
      
      surveyQs_Mis_full <- rbind(surveyQs_Mis_full, datMis_full)
    }
    
  }
  
  
  surveyQs_2plot <-
   surveyQs_No %>%
    mutate(model = "base") %>%
    bind_rows({surveyQs_Mis %>% mutate(model = "with misreporting")}) %>%
    mutate(scenario = as.character(scenario),
           scenario = ifelse(scenario == "rw", "random walk", scenario),
           scenario = ifelse(scenario == "rw10", "random walk 10yrs", scenario),
           scenario = paste(scenario, "scenario"),
           scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                   "random walk scenario",
                                                   "random walk 10yrs scenario",
                                                   "misspecified M scenario"))) %>%
    gather(age, logQ, -scenario, -rep, -leaveOutYears, -survey, -model) %>%
    mutate(age = paste0("age-", substr(age, 2,2)),
           Q = exp(logQ)) %>%
    group_by(scenario, leaveOutYears, age, model) %>%
    summarise(Q_median = median(Q, na.rm = T),
              Q_hi  = quantile(Q, 0.75, na.rm = T),
              Q_lo  = quantile(Q, 0.25, na.rm = T)) %>%
    mutate(dataset = "loo") %>%
    bind_rows({bind_rows({surveyQs_No_full %>% mutate(model = "base")},
                         {surveyQs_Mis_full %>% mutate(model = "with misreporting")}) %>%
        mutate(scenario = as.character(scenario),
               scenario = ifelse(scenario == "rw", "random walk", scenario),
               scenario = ifelse(scenario == "rw10", "random walk 10yrs", scenario),
               scenario = paste(scenario, "scenario"),
               scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                       "random walk scenario",
                                                       "random walk 10yrs scenario",
                                                       "misspecified M scenario"))) %>%
        gather(age, logQ, -scenario, -rep, -survey, -model) %>%
        mutate(age = paste0("age-", substr(age, 2,2)),
               Q = exp(logQ)) %>%
        group_by(scenario, age, model) %>%
        summarise(Q_median = median(Q, na.rm = T),
                  Q_hi  = quantile(Q, 0.75, na.rm = T),
                  Q_lo  = quantile(Q, 0.25, na.rm = T)) %>%
        mutate(dataset = "full")})

    p <-
      ggplot(data = surveyQs_2plot,
             aes(x = leaveOutYears)) +
      geom_line(data = surveyQs_2plot %>% filter(dataset == "loo"),
                aes(y = Q_median, color = model), linetype = 1) +
      geom_ribbon(data = surveyQs_2plot %>% filter(dataset == "loo"),
                  aes(ymin = Q_lo, ymax = Q_hi, fill = model),
                  alpha = 0.3) +
      geom_hline(data = surveyQs_2plot %>% filter(dataset == "full"),
                 aes(yintercept = Q_median, color = model), linetype = 2) +
      facet_wrap(age ~ scenario, scales = "free", ncol = 4) +
      theme_bw() +
      theme(axis.title   = element_text(size = 14),
            plot.title   = element_text(size = 16),
            strip.text   = element_text(size = 9)) +
      xlab("Leave-out year") +
      ylab("Q estimate") +
      #scale_color_manual(values = c("black", "red")) +
      ggtitle("loo vs full dataset Q estimates") +
      geom_vline(data = scaled_yearsSimStart, aes(xintercept = start))
    plot(p)

  
}











