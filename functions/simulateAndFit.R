simulateAndFit <- function(sim_label, ...){
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
                               noScaledYears = noScaledYearsFit,
                               sim_label = sim_label[i,]) 
    
    setupMis[[i]] <- setupModel(conf = fitNScod$conf,
                                stock_dir = "simData",
                                misreportingType = "rw",
                                noScaledYears = noScaledYearsFit,
                                sim_label = sim_label[i,])
  }
  
   
    # Debugging code:  
    #ind = 1
    #setupMis[[ind]]$par$itrans_rhoS <- 0
    #fit_mis = fit(setupMis[[ind]]$dat, setupMis[[ind]]$conf, setupMis[[ind]]$par, "with_misreporting")#, map = list(itrans_rhoS = factor(NA)))
    #fit_bas = fit(setupNo[[ind]]$dat, setupNo[[ind]]$conf, setupNo[[ind]]$par, "base")
   #x=setupNo[[ind]]# << TEMP TO DEBUG
   #x1 = fit(x$dat, x$conf, x$par, "base")
   # See if M is misinterpreted as misreporting
   #fit_mis$sdrep
   #trans <- function(x) 2/(1 + exp(-2 * x)) - 1
   #trans(fit_mis$sdrep$par.fixed["itrans_rhoS"])
   #fit_bas$sdrep
   # #simOut[[1]]$trueParams$pl$logS %>% exp()
   # #x1$pl$logS %>% exp()
   #plotReTsAtAge(fit_mis, simOut[[ind]])
   # plotReTsAtAge(fit_bas, simOut[[ind]])
   # fitVsObsBase <- extractFitVsObs(fit = fit_mis, model = "with_misreporting")
   # plotFitVsObs(fitVsObsBase)
   # 
   # AIC(fit_mis)
   # AIC(fit_bas)
   #  
   # plot(residuals(fit_bas))
   # plot(stockassessment2:::residuals.sam(fit_mis))
   # 
   # plot(procres(fit_bas))
   # plot(stockassessment2::procres(fit_mis))
   #
   #out=try(fitLO(x$dat, x$conf, x$par, "base_LO", k = k))#, map = list(logitFracMixS = factor(NA)))
  
  # Fit model
  cl <- makeCluster(detectCores() - 1) #setup nodes for parallel
  #load stockassessment and functions to each node
  clusterEvalQ(cl, {library(stockassessment); source("loadFunctions.R")}) 
  
  # Estimate with misreporting as random effect
  fitNoSim <- parLapply(cl, setupNo, function(x){try(fit(x$dat, x$conf, x$par, "base"))})
  fitMisSim <- parLapply(cl, setupMis, function(x){try(fit(x$dat, x$conf, x$par, "with_misreporting"))})
  
  fitNoSimLO <- parLapply(cl, setupNo, function(x){try(fitLO(x$dat, x$conf, x$par, "base_LO"))})
  fitMisSimLO <- parLapply(cl, setupMis, function(x){try(fitLO(x$dat, x$conf, x$par, "with_misreporting_LO"))})
  
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
  
  return(list(simOutAccept = simOutAccept, 
              setupNoAccept = setupNoAccept,
              setupMisAccept = setupMisAccept,
              fitNoSimAccept = fitNoSimAccept,
              fitMisSimAccept = fitMisSimAccept,
              fitNoSimLOAccept = fitNoSimLOAccept,
              fitMisSimLOAccept = fitMisSimLOAccept,
              sim_labelAccept = sim_labelAccept))
}

