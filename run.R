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

library(MASS)
library(stockassessment2) # modified SAM 
library(stockassessment) # original SAM 
library(dplyr)
library(ggplot2)
library(tidyr)
library(parallel)
library(purrr)


# Load functions
source("loadFunctions.R")


## SIMULATED DATA ANALYSIS ##########################################

#### Configure simulations ####
noScaledYearsSim <- 10
noScaledYearsFit <- 20
scenarios <- c("uniform random", 
               "rw",
               "fixed",
               "no misreporting")
nRep <- 150#300
sim_label <- expand.grid(replicate = 1:nRep, 
                         scenario = scenarios, 
                         stringsAsFactors = F)

#### RUN SIMULATIONS AND FIT MODEL(S) ####
simsAndFits <- simulateAndFit(noScaledYearsSim, sim_label)

fitMisSimAccept <- simsAndFits$fitMisSimAccept
simOutAccept <- simsAndFits$simOutAccept
setupAccept <- simsAndFits$setupAccept


#### Calculate error ####
err <- calcErr(fitMisSimAccept, simOutAccept)

#### Calculate confusion table ####
confusionTables <- calcConfusion(err)

## Make plots ##
plots(fitMisSimAccept, simOutAccept, err)


## REAL DATA ANALYSIS ###############################################

# Load data and setup SAM model configurations
# setupBase <- setupModel(stock_dir = "GOMcod",
#                         misreportingType = "no misreporting")
# setupMis <- setupModel(stock_dir = "GOMcod",
#                        misreportingType = "rw",
#                        noScaledYears = noScaledYearsFit)
# 
# # Fit models
# fitBaseReal <- sam.fit(setupBase$dat, setupBase$conf, setupBase$par)
# 
# fitMisReal <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)
# 
# # Compare models
# compareTs(fitBaseReal, fitMisReal)
# 
# # Plot example random effects at age
# plotReTsAtAge(fitMisReal)
# 
# 
# # Estimated misreporting correlation
# transf <- function(x) 2/(1 + exp(-2 * x)) - 1
# (ar1coef <- transf(fitMisReal$pl$itrans_rhoS))
