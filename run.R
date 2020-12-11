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
noScaledYearsSim <- 20
noScaledYearsFit <- 20
scenarios <- c(#"uniform random", 
               "rw"#,
               #"fixed",
               #"no misreporting",
               #"misspecified M"
  )
seed <- sample(1:1000, 1)
set.seed(seed)
nRep <- 50#300
sim_label <- expand.grid(replicate = 1:nRep, 
                         scenario = scenarios, 
                         stringsAsFactors = F)

#### RUN SIMULATIONS AND FIT MODELS ####
simsAndFits <- simulateAndFit(noScaledYearsSim, sim_label)

simOutAccept <- simsAndFits$simOutAccept
fitNoSimAccept <- simsAndFits$fitNoSimAccept
fitMisSimAccept <- simsAndFits$fitMisSimAccept
fitNoSimLOAccept <- simsAndFits$fitNoSimLOAccept
fitMisSimLOAccept <- simsAndFits$fitMisSimLOAccept
setupNoAccept <- simsAndFits$setupNoAccept
setupMisAccept <- simsAndFits$setupMisAccept
sim_labelAccept <- simsAndFits$sim_labelAccept

#### Calculate fit error ####
errNo  <- calcErr(fitNoSimAccept,  simOutAccept)
errMis <- calcErr(fitMisSimAccept, simOutAccept)
err <- rbind(errNo, errMis)

#### Calculate leave-out error ####
errLO <- pmap_dfr(list(fitNoSimLOAccept, fitMisSimLOAccept, 
                       simOut = simOutAccept, setup = setupNoAccept), calcErrLO)

# Save sims, fits, and errors
save(list = c("seed",
              "simOutAccept",
              "fitNoSimAccept",
              "fitMisSimAccept",
              "fitNoSimLOAccept",
              "fitMisSimLOAccept",
              "sim_labelAccept",
              "err",
              "errLO"),
     file = paste0("./output/simsFitsAndErr", paste0(Sys.Date(), ".Rdata")))

#### Calculate confusion table ####
# confusionTables <- calcConfusion(errMis)

## Make plots ##
#plots(fitMisSimAccept, simOutAccept, err, type = "fit", plotScale = F)

# Plot mean fit vs mean true for each scenario
#plotMeanFitVTru(errLO, simOutAccept[[1]])

# Plot an example random effects at age
# plotReTsAtAge(fitMisSimAccept[[1]], simOutAccept[[1]])

# Plot time series fit error
# plotTsError(err,
#             type = "Not LO",
#             scaled_yearsFit = fitMisSimAccept[[1]]$conf$keyScaledYears,
#             scaled_yearsSim = simOutAccept[[1]]$scaled_yearsSim,
#             plotScale = F)

## Plot LO error comparison ##
# Plot LOO CV error on survey observations #
# plotSurveyError(errLO,
#                 type = "LO",
#                 scaled_yearsSim = simOutAccept[[1]]$scaled_yearsSim)

# Plot LOO CV error on unobserved variables #
# plotTsError(errLO,
#             type = "LO",
#             scaled_yearsFit = fitMisSimAccept[[1]]$conf$keyScaledYears,
#             scaled_yearsSim = simOutAccept[[1]]$scaled_yearsSim,
#             plotScale = F)

# Plot mean fit vs tru for LOO prediction
#plotMeanFitVTru(errLO, simOutAccept[[1]])

## Calculate one-step-ahead residuals ##
#res <- calcResAll(fitNo = fitNoSimAccept, fitMis = fitMisSimAccept, sim_labelAccept)

# Plot residual error
#plotRes(res, simOutAccept)


## REAL DATA ANALYSIS ###############################################

# # Load data and setup SAM model configurations
# setupNo <- setupModel(stock_dir = "GOMcod",
#                       misreportingType = "no misreporting")
# setupMis <- setupModel(stock_dir = "GOMcod",
#                        misreportingType = "rw",
#                        noScaledYears = noScaledYearsFit)
# 
# # Haddock: Replace first zero with small value
# logobsOrig <- setupNo$dat$logobs
# setupNo$dat$logobs[which(is.na(logobsOrig))[1]] <- log(0.1)
# 
# base <- sam.fit(setupNo$dat, setupNo$conf, setupNo$par)
# 
# # Haddock: Replace first zero and turn off variability in scale
# logobsOrig <- setupMis$dat$logobs
# setupMis$dat$logobs[which(is.na(logobsOrig))[1]] <- log(0.1)
# setupMis$par$logitFracMixS <- -100
# setupMis$par$itrans_rhoS <- 0
# setupMis$par$logSdLogSsta <- -10
# # Set map for haddock only
# mapMis = list("logitFracMixS" = factor(NA), "itrans_rhoS" = factor(NA), "logSdLogSsta" = factor(NA))
# 
# #setupMisHalf <- setupMis
# #setupMisHalf$dat$logobs[setupMisHalf$dat$aux[,"fleet"]]
# 
# with_misreporting <- sam.fit_cp(setupMis$dat, setupMis$conf, setupMis$par)#, map = mapMis)
# 
# 
# # Perform LOO fits
# fitNoLO <- fitLO(setupNo$dat, setupNo$conf, setupNo$par, "base_LO")
# fitMisLO <- fitLO(setupMis$dat, setupMis$conf, setupMis$par, "with_misreporting_LO")#, map = mapMis)
# 
# 
# # Calculate LOO error
# errLOReal <- calcErrLO(fitNo = fitNoLO, fitMis = fitMisLO, setup = setupNo, scenario = "GOM cod", replicate = NA)
# 
# # Plot LOO CV error on survey observations #
# plotSurveyError(errLOReal, type = "LO")
# 
# 
# # Plot Fit vs Observed for each model
# fitVsObsBase <- extractFitVsObs(with_misreporting, model = "with misreporting")
# plotFitVsObs(fitVsObsBase)
# 
# d2plot <- 
#   fitVsObsBase %>%
#   mutate(resLogobs = logobsFit - logobs,
#          resObs = obsFit - obs) %>%
#   group_by(year, fleetName, model)
# 
# ggplot(d2plot %>% filter(fleetName == "Residual catch"), aes(x=year)) +
#   geom_line(aes(y=resLogobs)) +
#   geom_vline(aes(xintercept =max(d2plot$year) - noScaledYearsFit + 1)) +
#   geom_hline(aes(yintercept = 0)) +
#   facet_wrap(~age) +
#   ylab("log-catch residual (obs - fit)") +
#   ggtitle("Catch fit residuals")
# 
# 
# # Compare models
# compareTs(base, with_misreporting)
# 
# 
# # Plot example random effects at age
# plotReTsAtAge(base)
# plotReTsAtAge(with_misreporting)
# 
# 
# # Estimated misreporting correlation
# transf <- function(x) 2/(1 + exp(-2 * x)) - 1
# (ar1coef <- transf(0))
# 
# # # Calculate one-step-ahead psuedoresiduals
# # resMisReal <- 
# #   calcRes(fit = with_misreporting, sim_label = data.frame(scenario = "GOM haddock",
# #                                                    replicate = 1)) %>%
# #   mutate(model = "with misreporting")
# # 
# # resNoReal <- 
# #   calcRes(fit = base, sim_label = data.frame(scenario = "GOM haddock",
# #                                                           replicate = 1)) %>%
# #   mutate(model = "base")
# # 
# # resReal <- rbind(resNoReal, resMisReal)
# # 
# # # Plot residual error
# # plotRes(resReal, simOutAccept, noScaledYearsFit)
# # 
# # # resMisReal2 <- stockassessment2:::residuals.sam(fitMisReal)
# # # resNoReal2 <- stockassessment2:::residuals.sam(fitNoReal_missing) 
# # # 
# # # plot(resMisReal2)
# # # plot(resNoReal2)
# # 
# # retroNo <- stockassessment::retro(base, year = 7)
# # 
# # 
# # retroMis <- retro_cp(with_misreporting, year = 7)#, map = list("logitFracMixS" = factor(NA),
# #                                                   #  "itrans_rhoS" = factor(NA),
# #                                                    # "logSdLogSsta" = factor(NA)))
# # plot(retroMis)
# # plot(retroNo)
# # 
# # mohn(retroMis)
# # mohn(retroNo)
# 
# 
