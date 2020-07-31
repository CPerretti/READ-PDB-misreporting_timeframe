# Project to evalute methods for estimating the timeframe of
# misreporting using a modified version of SAM.
#

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
set.seed(321)
nRep <- 1#100#300
sim_label <- expand.grid(replicate = 1:nRep, 
                         scenario = scenarios, 
                         stringsAsFactors = F)

#### RUN SIMULATIONS AND FIT MODELS ####
simsAndFits <- simulateAndFit(noScaledYearsSim, sim_label, k = 52)

simOutAccept <- simsAndFits$simOutAccept
fitNoSimAccept <- simsAndFits$fitNoSimAccept
fitMisSimAccept <- simsAndFits$fitMisSimAccept
fitNoSimLOAccept <- simsAndFits$fitNoSimLOAccept
fitMisSimLOAccept <- simsAndFits$fitMisSimLOAccept
sim_labelAccept <- simsAndFits$sim_labelAccept

#### Calculate fit error ####
errNo  <- calcErrTru(fitNoSimAccept,  simOutAccept)
errMis <- calcErrTru(fitMisSimAccept, simOutAccept)
err <- rbind(errNo, errMis)

#### Calculate leave-out error ####
errLO <- pmap_dfr(list(fitNoSimLOAccept, fitMisSimLOAccept, simOutAccept), calcErrTruLO)

# Save sims, fits, and errors
save(list = c("simOutAccept",
              "fitNoSimAccept",
              "fitMisSimAccept",
              "sim_labelAccept",
              "err",
              "errLO"), 
     file = paste0("./output/simsFitsAndErr", paste0(Sys.Date(), ".Rdata")))

#### Calculate confusion table ####
confusionTables <- calcConfusion(errMis)

## Make plots ##
plots(fitMisSimAccept, simOutAccept, err)

## Plot LO error comparison ##
plotTsError(errLO, 
            scaled_yearsFit = fitMisSimAccept[[1]]$conf$keyScaledYears, 
            scaled_yearsSim = simOutAccept[[1]]$scaled_yearsSim)


## Calculate one-step-ahead residuals ##
#res <- calcResAll(fitNo = fitNoSimAccept, fitMis = fitMisSimAccept, sim_labelAccept)

# Plot residual error
#plotRes(res, simOutAccept)


## REAL DATA ANALYSIS ###############################################
fitModelsReal(stock_dir = "GOMcod")
# Load data and setup SAM model configurations
setupNo <- setupModel(stock_dir = "GOMcod",
                      misreportingType = "no misreporting")
setupMis <- setupModel(stock_dir = "GOMcod",
                       misreportingType = "rw",
                       noScaledYears = noScaledYearsFit)

# Fit models



# Haddock: Replace first zero with small value
logobsOrig <- setupNo$dat$logobs
setupNo$dat$logobs[which(is.na(logobsOrig))[1]] <- log(0.1)
base <- sam.fit(setupNo$dat, setupNo$conf, setupNo$par)


# Haddock: Replace first zero and turn off variability in scale
logobsOrig <- setupMis$dat$logobs
setupMis$dat$logobs[which(is.na(logobsOrig))[1]] <- log(0.1)
setupMis$par$logitFracMixS <- -100
setupMis$par$itrans_rhoS <- 0
setupMis$par$logSdLogSsta <- -10

with_misreporting <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)#,
                        # map = list("logitFracMixS" = factor(NA),
                        #          "itrans_rhoS" = factor(NA),
                        #           "logSdLogSsta" = factor(NA)))#,
                        #             # "logS" = factor(matrix(NA, 
                                    #                        nrow = nrow(setupMis$par$logS),
                                    #                        ncol = ncol(setupMis$par$logS)))))


# Plot Fit vs Observed for each model
fitVsObsBase <- extractFitVsObs(with_misreporting, model = "with misreporting")
plotFitVsObs(fitVsObsBase)

d2plot <- 
  fitVsObsBase %>%
  mutate(resLogobs = logobsFit - logobs,
         resObs = obsFit - obs) %>%
  group_by(year, fleetName, model)

ggplot(d2plot %>% filter(fleetName == "Residual catch"), aes(x=year)) +
  geom_line(aes(y=resLogobs)) +
  geom_vline(aes(xintercept =max(d2plot$year) - noScaledYearsFit + 1)) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~age) +
  ylab("log-catch residual (obs - fit)") +
  ggtitle("Catch fit residuals")


# Compare models
compareTs(base, with_misreporting)


# Plot example random effects at age
plotReTsAtAge(base)
plotReTsAtAge(with_misreporting)


# Estimated misreporting correlation
transf <- function(x) 2/(1 + exp(-2 * x)) - 1
(ar1coef <- transf(0))

# Calculate one-step-ahead psuedoresiduals
resMisReal <- 
  calcRes(fit = with_misreporting, sim_label = data.frame(scenario = "GOM haddock",
                                                   replicate = 1)) %>%
  mutate(model = "with misreporting")

resNoReal <- 
  calcRes(fit = base, sim_label = data.frame(scenario = "GOM haddock",
                                                          replicate = 1)) %>%
  mutate(model = "base")

resReal <- rbind(resNoReal, resMisReal)

# Plot residual error
plotRes(resReal, simOutAccept, noScaledYearsFit)

# resMisReal2 <- stockassessment2:::residuals.sam(fitMisReal)
# resNoReal2 <- stockassessment2:::residuals.sam(fitNoReal_missing) 
# 
# plot(resMisReal2)
# plot(resNoReal2)

retroNo <- stockassessment::retro(base, year = 7)


retroMis <- retro_cp(with_misreporting, year = 7)#, map = list("logitFracMixS" = factor(NA),
                                                  #  "itrans_rhoS" = factor(NA),
                                                   # "logSdLogSsta" = factor(NA)))
plot(retroMis)
plot(retroNo)

mohn(retroMis)
mohn(retroNo)


