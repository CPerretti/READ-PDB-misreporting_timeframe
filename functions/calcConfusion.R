calcConfusion <- function(err) {
  # Confusion table
  TrueGivenPrediction <- 
    err %>%
    filter(variable == "S") %>%
    group_by(scenario) %>%
    count(predictMisreporting95, trueMisreporting) %>%
    group_by(scenario, predictMisreporting95) %>%
    mutate(perc = (100 * n / sum(n)) %>% round(1)) %>%
    select(-n) %>% 
    spread(trueMisreporting, perc,  fill = 0) %>%
    rename(`95% CI prediction` = predictMisreporting95,
           `Percent true No misreporting` = `No misreporting`,
           `Percent true Yes misreporting` = `Yes misreporting`) %>%
    ungroup() %>%
    rename(Scenario = scenario) %>%
    mutate(Scenario = factor(Scenario, levels = c("no misreporting scenario",
                                                  "fixed scenario",
                                                  "random walk scenario",
                                                  "uniform random scenario")))
  
  PredictionGivenTrue <-
    err %>%
    filter(variable == "S") %>%
    group_by(scenario) %>%
    count(predictMisreporting95, trueMisreporting) %>%
    group_by(scenario, trueMisreporting) %>%
    mutate(perc = (100 * n / sum(n)) %>% round(1)) %>%
    select(-n) %>% 
    spread(predictMisreporting95, perc,  fill = 0) %>%
    rename(`Truth` = trueMisreporting,
           `Percent predicted No misreporting` = `No misreporting`,
           `Percent predicted Yes misreporting` = `Yes misreporting`) %>%
    ungroup() %>%
    rename(Scenario = scenario) %>%
    mutate(Scenario = factor(Scenario, levels = c("no misreporting scenario",
                                                  "fixed scenario",
                                                  "random walk scenario",
                                                  "uniform random scenario")))
  return(confusionTables <- list(TrueGivenPrediction = TrueGivenPrediction, 
                                 PredictionGivenTrue = PredictionGivenTrue))
}