simulateAndFit <- function(noScaledYearsSim, sim_label, k, ...){
  # Load NScod example to configure simulations
  load("./wg_MGWG/state-space/simData/fitNScod.Rdata")
  
  # Setup simulation scenarios
  simOut    <- vector("list", length = nrow(sim_label))
  setupNo   <- vector("list", length = nrow(sim_label))
  setupMis  <- vector("list", length = nrow(sim_label))
  
  unifLower <- 1.5
  unifUpper <- 10
  # Choose initial distribution parameters for rw that generate a time
  # series mean and variance that most closely matches the other
  # two misreporting scenarios.
  opt_par <-optim(par = c(log(1), log(0.2)), # starting mu_c = exp(par[1]), sd_c = 0.001 + exp(par[2])
                  fn = initsObj, 
                  tsmean_target = 0.5 * (unifUpper + unifLower),
                  tsvar_target = 1/12 * (unifUpper - unifLower)^2, 
                  T = noScaledYearsSim)
  mu_c <- exp(opt_par$par[1]) # lower bound of 0
  sd_c <- 0.001 + exp(opt_par$par[2]) # lower bound of 0.001
  # Generate simulations
  for (i in 1:nrow(sim_label)) {
    simOut[[i]] <-
      sim(fit = fitNScod,
          noScaledYears = noScaledYearsSim,
          sim_label = sim_label[i,],
          unifLower = unifLower, 
          unifUpper = unifUpper,
          mu_c = mu_c, 
          sd_c = sd_c)
    
    # Prep simulation data for read.ices()
    prepSimData(simOut = simOut[[i]]) 
    
    # Load simulated data and setup SAM model configurations
    setupNo[[i]] <- setupModel(conf = fitNScod$conf,
                               stock_dir = "simData",
                               misreportingType = "no misreporting",
                               noScaledYears = noScaledYearsFit) 
    
    setupMis[[i]] <- setupModel(conf = fitNScod$conf,
                                stock_dir = "simData",
                                misreportingType = "rw",
                                noScaledYears = noScaledYearsFit)
  }
  
   # x=setupNo[[1]]# << TEMP TO DEBUG
   # out=try(fitLO(x$dat, x$conf, x$par, "base_LO", k = 3))#, map = list(logitFracMixS = factor(NA)))# << TEMP TO DEBUG
  
  # Fit model
  cl <- makeCluster(detectCores() - 1) #setup nodes for parallel
  #load stockassessment and functions to each node
  clusterEvalQ(cl, {library(stockassessment); source("loadFunctions.R")}) 
  
  # Estimate with misreporting as random effect
  fitNoSim <- parLapply(cl, setupNo, function(x){try(fit(x$dat, x$conf, x$par, "base"))})
  fitMisSim <- parLapply(cl, setupMis, function(x){try(fit(x$dat, x$conf, x$par, "with_misreporting"))})
  
  fitNoSimLO <- parLapply(cl, setupNo, function(x){try(fitLO(x$dat, x$conf, x$par, "base_LO", k))})
  fitMisSimLO <- parLapply(cl, setupMis, function(x){try(fitLO(x$dat, x$conf, x$par, "with_misreporting_LO", k))})
  
  stopCluster(cl) #shut down nodes
  
  # Error handling
  ind2keep0 <- which(sapply(fitNoSim, class)    != "try-error" & 
                     sapply(fitMisSim, class)   != "try-error")
  
  # also exclude non-convergences
  ind2keep1 <- ind2keep0[unlist(sapply(fitNoSim[ind2keep0], 
                                    function (x) x[[6]][3])) != 1 &
                         unlist(sapply(fitMisSim[ind2keep0], 
                                    function (x) x[[6]][3])) != 1]
  
  ind2keep2 <- vector()
  for (i in ind2keep1) {
    if (mean(exp(fitNoSim[[i]]$pl$logN) / 
             exp(simOut[[i]]$trueParams$pl$logN)) < 1000 &
        mean(exp(fitMisSim[[i]]$pl$logN) / 
             exp(simOut[[i]]$trueParams$pl$logN)) < 1000) {
      ind2keep2 <- c(ind2keep2, i)
    }
  }
  
  simOutAccept      <- simOut[ind2keep2]
  setupNoAccept     <- setupNo[ind2keep2]  
  setupMisAccept    <- setupMis[ind2keep2]
  fitNoSimAccept    <- fitNoSim[ind2keep2]
  fitMisSimAccept   <- fitMisSim[ind2keep2]
  fitNoSimLOAccept  <- fitNoSimLO[ind2keep2]
  fitMisSimLOAccept <- fitMisSimLO[ind2keep2]
  sim_labelAccept   <- sim_label[ind2keep2,]
  
  # Save fits and setups
  suffix <- paste0(Sys.Date(), ".Rdata")
  save(list = c("simOutAccept",
                "setupNoAccept",
                "setupMisAccept",
                "fitNoSimAccept",
                "fitMisSimAccept",
                "fitNoSimLOAccept",
                "fitMisSimLOAccept",
                "sim_labelAccept"), 
       file = paste0("./output/setupAndFits", suffix))
  
  return(list(simOutAccept = simOutAccept, 
              setupNoAccept = setupNoAccept,
              setupMisAccept = setupMisAccept,
              fitNoSimAccept = fitNoSimAccept,
              fitMisSimAccept = fitMisSimAccept,
              fitNoSimLOAccept = fitNoSimLOAccept,
              fitMisSimLOAccept = fitMisSimLOAccept,
              sim_labelAccept = sim_labelAccept))
}

