makeTableErr <- function(err) {
  tableOfError <-
    err %>%
    filter(variable %in% c("S"),
           year %in% c(simOutAccept[[1]]$scaled_yearsSim[1] - 1,
                       simOutAccept[[1]]$scaled_yearsSim[1])) %>%
    group_by(model, scenario, variable) %>%
    summarise(mean_mape = mean(abs_error_pc)) %>%
    spread(model, mean_mape) %>%
    mutate(variable = ifelse(variable == "S", "Scale", variable),
           `p0 - pfit` = p0 - pfit) %>%
    left_join({map2_dfr(fitMisSimAccept, simOutAccept,
                       function(fit, simOut){
                         data.frame(scenario = simOut$sim_label$scenario,
                                    replicate = simOut$sim_label$replicate,
                                    logitFracMixS = fit$sdrep$par.fixed["logitFracMixS"])}) %>%
                mutate(fracMixS = 1/(1 + exp(-logitFracMixS)),
                       scenario = ifelse(scenario == "rw", "random walk", scenario),
                       scenario = paste(scenario, "scenario"),
                       scenario  = factor(scenario, levels = c("no misreporting scenario",
                                                               "fixed scenario",
                                                               "random walk scenario",
                                                               "uniform random scenario"))) %>%
                group_by(scenario) %>%
                summarise(meanFracMixS = mean(fracMixS))})
}