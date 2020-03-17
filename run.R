# Project to evalute methods for estimating the timeframe of
# misreporting using a modified version of SAM.
#
# Tentative plan
# (x) Modify SAM to estimate correlated random walk misreporting 
# (x) Self-test new SAM model with correct misreporting timeframe
# (x) Self-test again but with incorrect misreporting timeframe to
#     see if it can identify the start of misreporting
# (x) Test in scenarios where it is misspecified
# ( ) Come up with some error statistics and calculate them
# ( ) Examine whether prediction metrics are useful for model selection
# ( ) Apply to real data perhaps using prediction metrics to choose model
# ( ) Examine how different our conclusions would be if we estimated
#     misreporting

# Install local forked version of package with changes
#devtools::install_local("../SAM2/stockassessment/", force = TRUE)
# Install forked version from github
#devtools::install_github("perretti/SAM2/stockassessment/", force = TRUE)

library(stockassessment2) # modified SAM 
library(stockassessment) # original SAM 
library(dplyr)
library(ggplot2)
library(tidyr)

# Load functions
source("loadFunctions.R")

# Number of scaled years
noScaledYearsSim <- 10
noScaledYearsFit <- 20

## SIMULATED DATA ANALYSIS ##########################################
# Configure simulations
# Load NScod example to configure simulations
load("./wg_MGWG/state-space/simData/fitNScod.Rdata")

# Generate simulations
simOut <-
  sim(fit = fitNScod,
      noScaledYears = noScaledYearsSim,
      scenario = "uniform random")

# Prep simulation data for read.ices()
prepSimData(simOut = simOut) 

# Load simulated data and setup SAM model configurations
setupMis <- setupModel(conf = fitNScod$conf,
                       stock_dir = "simData",
                       misreportingType = "rw",
                       noScaledYears = noScaledYearsFit)

# Fit models
fitMisSim <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)

# Plot random effects at age
plotReTsAtAge(fitMisSim, simOut)

# ## REAL DATA ANALYSIS ###############################################

# Load data and setup SAM model configurations
setupBase <- setupModel(stock_dir = "GOMcod",
                        misreportingType = "no misreporting")
setupMis <- setupModel(stock_dir = "GOMcod",
                       misreportingType = "rw",
                       noScaledYears = noScaledYearsFit)

# Fit models
fitBaseReal <- sam.fit(setupBase$dat, setupBase$conf, setupBase$par)

fitMisReal <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)

# Compare models
compareTs(fitBaseReal, fitMisReal)

# AIC(fitBase)
# AIC(fitMis)

# Plot random effects at age
plotReTsAtAge(fitMisReal)


# Estimated misreporting correlation
transf <- function(x) 2/(1 + exp(-2 * x)) - 1
(ar1coef <- transf(fitMisReal$pl$itrans_rhoS))
