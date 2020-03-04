# Project to evalute methods for estimating the timeframe of
# misreporting using a modified version of SAM.
#
# Tentative plan
# (x) Modify SAM to estimate correlated random walk misreporting 
# ( ) Self-test new SAM model with correct misreporting timeframe
# ( ) Self-test again but with incorrect misreporting timeframe to
#     see if it can identify the start of misreporting
# ( ) Test in scenarios where it is misspecified
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
source("load_functions.R")


# Load data and setup SAM model configurations
setupBase <- setupModel(stock_dir = "GOMcod", 
                        misreportingType = "no misreporting")
setupMis <- setupModel(stock_dir = "GOMcod", 
                       misreportingType = "rw")

# Fit models
fitBase <- sam.fit(setupBase$dat, setupBase$conf, setupBase$par)

fitMis <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)
                     #map = list("itrans_rhoS" = factor(NA)))
  
# Compare models
compareTs(fitBase, fitMis)


AIC(fitBase)
AIC(fitMis)

# Estimated misreporting correlation
transf <- function(x) 2/(1 + exp(-2 * x)) - 1
ar1coef <- transf(fitMis$pl$itrans_rhoS)

