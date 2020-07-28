calcResAll <- function(fitNo, fitMis, sim_labelAccept) {
  resNo <- map2_dfr(fitNo, 
                    split((sim_labelAccept), seq(nrow((sim_labelAccept)))), 
                    calcRes) %>%
    mutate(model = "no misreporting")
  
  resMis <- map2_dfr(fitMis, 
                     split((sim_labelAccept), seq(nrow((sim_labelAccept)))), 
                     calcRes) %>%
    mutate(model = "random walk")
  
  res <- rbind(resNo, resMis)
  
  # suffix <- paste0(Sys.Date(), ".Rdata")
  # save(list = c("res"), 
  #      file = paste0("./output/res", suffix))
  
  return(res)
}

