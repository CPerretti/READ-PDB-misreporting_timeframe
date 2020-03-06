diffTs <- function(base, `with misreporting`) {
  diffs <-
    rbind(extractTsSdrep(base), 
          extractTsSdrep(`with misreporting`)) %>%
    group_by(variable, year, variableLabel) %>%
    mutate(diffRaw = value[model == substitute(`with misreporting`)] - 
             value[model == substitute(base)],
           diffPc  = 100 * diffRaw / value[model == substitute(base)])
}