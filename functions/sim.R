## Simulation model #######################################
sim <- function(fit, noScaledYears, sim_label,
                unifLower, unifUpper, mu_c, sd_c) {
  
  fit$conf$constRecBreaks <- numeric(0) # Needed for new SAM
  keyLogScale <- fit$conf$keyLogFsta
  keyLogScale[keyLogScale > -1] <- 0:(length(keyLogScale[keyLogScale > -1])-1)
  
  nA <- ncol(fit$data$propF) # number of age-classes
  nT <- fit$data$noYears # length of time series
  
  
  # Setup keyLogScale to have unique logScale for each age that is fished
  nAs <- sum(keyLogScale[1,] > -1)
  
  switch(sim_label$scenario,
         `uniform random` = {
           logS <- matrix(data = log(runif(nAs * noScaledYears, 
                                           unifLower, unifUpper)),
                              nrow = nAs, ncol = noScaledYears)
           # logS <- matrix(data = log(rnorm(nAs * noScaledYears, 8, 2)),
           #                    nrow = nAs, ncol = noScaledYears)
         },
         `rw` = { # correlated RW
           logSdLogScale <- log(sd_c)
           rw_logS_mat <- matrix(data = NA, nrow = nAs, ncol = noScaledYears)
           # errS <- matrix(data = rnorm(nAs * noScaledYears, 0, exp(logSdLogScale)),
           #                nrow = nAs, ncol = noScaledYears) #uncorrelated error
           
           # Setup correlation matrix for correlated error 
           scor = matrix(NA, nrow = nAs, ncol = nAs)
           diag(scor) <- 1
           rho <- 0.5
           for(i in 1:nAs){
             for(j in 1:i){
               scor[i,j] <- rho^abs(i-j)
               scor[j,i] <- scor[i,j]
             }
           } 
           
           svar <- exp(logSdLogScale)^2 * scor
        
           errS <- matrix(data = MASS::mvrnorm(n = noScaledYears, mu = rep(0, nAs), Sigma = svar),
                          ncol = nAs) %>% t() # correlated error
           
           rw_logS_mat[,1] <- mu_c + errS[,1]
           for(i in 2:noScaledYears){
             rw_logS_mat[,i] <- rw_logS_mat[,i-1] + errS[,i]
           }
           
           logS <- matrix(data = rw_logS_mat, nrow = nAs, ncol = noScaledYears)
         },
         fixed = { 
           # Misreporting on all ages
           logS <- matrix(data = rep(log(runif(1, unifLower, unifUpper)),
                                         times = nAs * noScaledYears),
                              nrow = nAs, ncol = noScaledYears)
           # Misreporting only on ages 1-3
           # logS <- matrix(data = rep(c(log(runif(1, 1.5, 10)), 0), 
           #                               each = nAs * noScaledYears / 2),
           #                    nrow = nAs, ncol = noScaledYears, byrow = T)
         },
         `no misreporting` = {
           logS <- matrix(data = log(1), nrow = nAs, ncol = noScaledYears)
         },
         `misspecified M` = { # misspecified M assumes no misreporting
           logS <- matrix(data = log(1), nrow = nAs, ncol = noScaledYears)
         }
  ) 
  
  # Set F (need to replicate some elements to match ModelConf)
  #f <- exp(fit$pl$logF[(fit$conf$keyLogFsta[1,] + 1),])
  # Calculate a new relization of f errors with sd's from the fit
  errF <- matrix(data = NA,
                 nrow = nA, 
                 ncol = nT - 1)
  
  # Set F sd
  fit$pl$logSdLogFsta <- 
    (c(1, rep(1, length(fit$pl$logSdLogFsta)-1)) * exp(fit$pl$logSdLogFsta)) %>%
    "*"(1) %>%
    log
  sdLogF <- exp(fit$pl$logSdLogFsta)
  for (i in 1:(nT-1)) { # Create F error
    errF[, i] <- rnorm(n = nA,
                       sd = sdLogF[fit$conf$keyVarF[1, ] + 1])[fit$conf$keyLogFsta[1, ] + 1]
  }
  
  # Simulate F
  logF <- matrix(data = NA, # F container
                 nrow = nA, 
                 ncol = nT, 
                 dimnames = list(paste0("tru.", c(1:nA)), fit$data$years)) 
  
  logF[, 1] <- rnorm(n = nA, 
                     mean = fit$pl$logF[, 1][fit$conf$keyLogFsta[1, ] + 1],
                     sd = fit$plsd$logF[, 1][fit$conf$keyLogFsta[1, ] + 1]) # initial F
  
  for (i in 2:nT) logF[, i] <- logF[, i - 1] + errF[, i - 1]
  
  f <- exp(logF)
  
  # Change M if doing misspecified M scenario
  if (sim_label$scenario == "misspecified M") {
    fit$data$natMor[(nrow(fit$data$natMor)-10+1):nrow(fit$data$natMor),] <- 
      2*fit$data$natMor[(nrow(fit$data$natMor)-10+1):nrow(fit$data$natMor),]
  }
  m <- t(fit$data$natMor)
  

  
  # Calcuate total mortality
  z <- f + m
  
  # Set up matrix to record N-at-age
  logN <- matrix(data = NA,
                 nrow = nA, 
                 ncol = nT, 
                 dimnames = list(paste0("tru.", c(1:nA)), fit$data$years)) 
  
  logN[, 1] <- rnorm(n = nA, 
                     mean = fit$pl$logN[, 1],
                     sd = fit$plsd$logN[, 1])# initial N
  
  # Calculate the process errors that were estimated in the fit so we can 
  # exactly replicate the fit
  # errPro_exact <- matrix(data = NA,
  #                        nrow = nA, 
  #                        ncol = nT-1)
  # 
  # errPro_exact[1, ] <- fit$pl$logN[1, 2:nT] - fit$pl$logN[1, 1:(nT-1)]
  # errPro_exact[-c(1, nA), ] <-  fit$pl$logN[-c(1, nA), 2:nT] -
  #   (fit$pl$logN[-c(nA-1, nA), 1:(nT-1)] -
  #      z[-c(nA-1, nA), 1:(nT-1)])
  # errPro_exact[nA, ] <- fit$pl$logN[nA, 2:nT] -
  #   log(exp(fit$pl$logN[nA-1, 1:(nT-1)]) *
  #         exp(-z[nA-1, 1:(nT-1)]) +
  #         exp(fit$pl$logN[nA, 1:(nT-1)]) *
  #         exp(-z[nA, 1:(nT-1)]))
  # Calculate a new relization of process errors with sd's from the fit
  # Set N process sd
  errPro <- matrix(data = NA,
                   nrow = nA, 
                   ncol = nT-1)
  # Possibly lower N process error (!)
  fit$pl$logSdLogN[(fit$conf$keyVarLogN + 1)] <-
    fit$pl$logSdLogN[(fit$conf$keyVarLogN + 1)] %>%
    exp %>% "*"(1) %>% log
  
  sdLogN <- exp(fit$pl$logSdLogN[(fit$conf$keyVarLogN + 1)])
  for (i in 1:(nT-1)) { # Create process error (N-at-age)
    errPro[, i] <-  rnorm(n = nA, sd = sdLogN)
  }
  
  #errPro <- errPro_exact # Use if you want the exact fit N
  
  ## Simulate population model #################################
  # N fit
  # Simulate N-at-age
  for (i in 2:nT) {
    logN[1, i] <- logN[1, i-1] + errPro[1, i-1]
    logN[-c(1, nA), i] <- logN[-c(nA-1, nA), i-1] - 
      z[-c(nA-1, nA), i-1] + 
      errPro[-c(1, nA), i-1]
    logN[nA, i] <- log(exp(logN[nA-1, i-1] - z[nA-1, i-1]) +
                         exp(logN[nA, i-1] - z[nA, i-1])) + errPro[nA, i-1]
  }
  
  N <- exp(logN)
  
  SSB <- (exp(logN)*exp(-exp(logF)*t(fit$data$propF)-t(fit$data$natMor)*t(fit$data$propM))*
            t(fit$data$propMat)*t(fit$data$stockMeanWeight)) %>% colSums
  ## Simulate observation model #############################
  # Observation error (for catch and surveys)
  errObs <- array(data = NA, # error container (3-d: age x year x fleet)
                  dim = c(nA, nT, fit$data$noFleets),
                  dimnames = list(paste0("error.", c(1:nA)), 
                                  fit$data$years, 
                                  attr(fit$data,"fleetNames")))
  # Need to replicate some sd's to match config file
  index <- as.vector(t(fit$conf$keyVarObs + 1))
  index[index == 0] <- NA
  fit$pl$logSdLogObs <- fit$pl$logSdLogObs %>% exp %>% "*"(1) %>% log
  sdLogObs <- exp(fit$pl$logSdLogObs[index]) 
  
  # Make observation error (can only do uncorrelated error right now)
  for (j in 1:nT) { # all surveys in a year
    errObs[, j, ] <- rnorm(n = length(sdLogObs), sd = sdLogObs)
  }
  
  # Check that sds are correctly assigned to each age x survey combo
  # sdInput <- c()
  # for (i in 1:fit$data$noFleets) {
  #   sdInput <- c(sdInput, apply(errObs[, , i], 1, sd))  
  # }
  # 
  # cbind(sdInput, sdLogObs)
  
  
  # Simulate Catch
  
  # Calculate catch on N-scale (1000s)
  #logCtru_N <- log(f / z * (1 - exp(-z)) * N)
  logCtru_N <- logN - log(z) + log(1 - exp(-z)) + logF
  rownames(logCtru_N) <- 1:nA
  
  logCobs_N0 <- logCtru_N + errObs[, , "Residual catch"]
  logCobs_N <- logCobs_N0
  logS_full <- cbind(matrix(data = 0, nrow = nrow(logS), ncol = nT - noScaledYears),
                         logS)
  
  for (a in 1:nA){
    if (fit$conf$keyLogFsta[1, a] > -1) { # If there is fishing on an age
      logCobs_N[a,] <- logCobs_N0[a,] - logS_full[keyLogScale[1,a] + 1,] # Misreport catch
    }
  }
  
  
  Ctru_N <- exp(logCtru_N)
  Cobs_N <- exp(logCobs_N) 
  
  # Convert to MT (1000s * kg = mt)
  Ctru_mt <- Ctru_N * t(fit$data$catchMeanWeight)
  Cobs_mt <- Cobs_N * t(fit$data$catchMeanWeight)
  
  
  # Simulate Survey
  logStru_N <- array(data = NA, # survey container (3-d: age x year x survey)
                     dim = c(nA, nT, fit$data$noFleets),
                     dimnames = list(c(1:nA), 
                                     fit$data$years, 
                                     attr(fit$data,"fleetNames")))
  
  logSobs_N <- logStru_N
  
  logSq <- matrix(data = NA, # survey q-at-age matrix
                  nrow = nrow(fit$conf$keyLogFpar), 
                  ncol = ncol(fit$conf$keyLogFpar))
  logSq[which(fit$conf$keyLogFpar != -1)] <- # fill with real fit values
    fit$pl$logFpar[fit$conf$keyLogFpar[fit$conf$keyLogFpar != -1] + 1]
  Sq <- exp(logSq)
  
  surveyIndex <- # some fleets are fishermen not surveys
    (1:fit$data$noFleets)[fit$data$fleetTypes == 2] 
  for (i in surveyIndex) {
    #logStru_N[, , i] <- log(Sq[i,] * exp(-z * fit$data$sampleTimes[i]) * N)
    logStru_N[, , i] <- logN - z * fit$data$sampleTimes[i] + logSq[i,]
    logSobs_N[, , i] <- logStru_N[, , i] + errObs[, , i]
  }
  
  # Remove survey observations where they are missing in the real data
  for (i in surveyIndex) {
    years2include <- unique(fit$data$aux[, "year"][fit$data$aux[, "fleet"] == i])
    logStru_N[, !(colnames(logStru_N[,,i]) %in% years2include), i] <- NA
    logSobs_N[, !(colnames(logStru_N[,,i]) %in% years2include), i] <- NA
  }
  
  Stru_N <- exp(logStru_N)
  Sobs_N <- exp(logSobs_N)
  
  
  
  
  trueParams <- list(data = fit$data, conf = fit$conf, 
                     sdrep = fit$sdrep, pl = fit$pl)
  trueParams$conf$keyScaledYears <- 
    (max(fit$data$years) - noScaledYears + 1):max(fit$data$years)
  trueParams$pl$logS <- logS_full
  if (sim_label$scenario == "random walk") trueParams$pl$logSdLogScale <- logSdLogScale
  trueParams$pl$logN <- logN
  dimnames(f) <- list(paste0("tru.", c(1:nA)), fit$data$years)
  trueParams$pl$logF <- logF
  return(list(trueParams = trueParams,
              scaled_yearsSim = (max(fit$data$years) - noScaledYears + 1):max(fit$data$years),
              N = N, 
              logN = logN,
              SSB = SSB,
              logCobs_N = logCobs_N,
              logCtru_N = logCtru_N, 
              logStru_N = logStru_N, 
              logSobs_N = logSobs_N,
              Cobs_mt = Cobs_mt, 
              Cobs_N = Cobs_N, 
              Ctru_mt = Ctru_mt, 
              Ctru_N = Ctru_N, 
              Sobs_N = Sobs_N, 
              Stru_N = Stru_N,
              mu_c = mu_c,
              sd_c = sd_c,
              sim_label = sim_label))
  
}