# Setup data and params for sam model #####################
compareTs <- function(base, `with misreporting`) {

  tsSdrep <- rbind(extractTsSdrep(base), extractTsSdrep(`with misreporting`))

  # Plot timeseries
  p <- 
    ggplot(tsSdrep, aes(x = year, y = value, color = model)) +
    geom_line() +
    geom_point(aes(y = obs), color = "black", shape = 4) +
    facet_wrap(~variableLabel, scales = "free_y") +
    theme_bw() + 
    ylab("")
  
  print(p)
  
  
  diffs <- diffTs(base, `with misreporting`)
  
  # Plot raw difference
  p <- 
    ggplot(diffs, aes(x = year, y = diffRaw)) +
    geom_line() +
    facet_wrap(~variableLabel, scales = "free") +
    ylab(paste0("Raw difference (",
                deparse(substitute(`with misreporting`)),
                " - ",
                deparse(substitute(base)),
                ")")) +
    theme_bw()
  
  
  
  print(p)
  
  # Plot percent difference
  p <-
    ggplot(diffs, aes(x = year, y = diffPc)) +
    geom_line() + 
    theme_bw() +
    facet_wrap(~variable, scales = "free") +
    ylab(paste0("Percent difference (",
                deparse(substitute(`with misreporting`)),
                " - ",
                deparse(substitute(base)),
                ")"))
  
  print(p)
}



