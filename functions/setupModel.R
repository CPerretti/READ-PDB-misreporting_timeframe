# Setup data and params for sam model #####################
setupModel <- function(conf = NULL, stock_dir, misreportingType = NULL,
                       noScaledYears = NULL, sim_label) {
  
  path_root <- paste0("./wg_MGWG/state-space/", stock_dir, "/")
  
  # Read in data
  cn <- read.ices(paste0(path_root, toupper(stock_dir), "_cn.dat")) # catch abundace-at-age
  sy <- read.ices(paste0(path_root, toupper(stock_dir), "_survey.dat")) #surveys
  cw <- read.ices(paste0(path_root, toupper(stock_dir), "_cw.dat")) # catch mean weight-at-age
  dw <- read.ices(paste0(path_root, toupper(stock_dir), "_dw.dat")) # discards mean weight-at-age
  lw <- read.ices(paste0(path_root, toupper(stock_dir), "_lw.dat")) # landings mean weight-at-age
  pf <- read.ices(paste0(path_root, toupper(stock_dir), "_pf.dat")) # proportion of f before spawning
  lf <- read.ices(paste0(path_root, toupper(stock_dir), "_lf.dat")) # fraction of catch that is landed 
  mo <- read.ices(paste0(path_root, toupper(stock_dir), "_mo.dat")) # maturity-at-age ogive
  nm <- read.ices(paste0(path_root, toupper(stock_dir), "_nm.dat")) # natural mortality-at-age
  pm <- read.ices(paste0(path_root, toupper(stock_dir), "_pm.dat")) # proportion of m before spawning
  sw <- read.ices(paste0(path_root, toupper(stock_dir), "_sw.dat")) # stock weight-at-age (kg)
  
  
  cn[which(cn== 0)] <- -1
  
  # setup the data as needed for SAM
  dat <- setup.sam.data(surveys = sy,
                        residual.fleet = cn,
                        prop.mature = mo,
                        stock.mean.weight = sw,
                        dis.mean.weight = dw,
                        land.mean.weight = lw,
                        land.frac = lf,
                        prop.f = pf,
                        prop.m = pm,
                        natural.mortality = nm,
                        catch.mean.weight = cw)
  
  # Load model configuration file
  if(is.null(conf)) conf <- loadConf(dat, paste0(path_root, "SAM/model.cfg"))
    
  # Set configuration if estimating misreporting
  keyLogScale <- conf$keyLogFsta #linking of scale timeseries
  keyLogScale[keyLogScale > -1] <- 0:(length(keyLogScale[keyLogScale > -1])-1)
  switch(misreportingType,
         `rw` = {
           conf$noScaledYears <- noScaledYears
           conf$keyLogScale <- keyLogScale
           conf$keyVarS <- conf$keyVarF #linking of scale variances
           conf$keyScaledYears <- (max(dat$years) - noScaledYears + 1):max(dat$years)
           conf$fracMixS <- 0
           conf$corFlagS <- 2 #type of misreporting correlation among ages
           # Turn off estimation of correlated misreporting when misreporting is not random walk.
           # This is because estimating correlation in other scenarios often results
           # in numerical problems which would cause it being turned off in practice.
           if (sim_label$scenario != "rw") conf$corFlagS <- 0
              },
         fixed = {
           conf$noScaledYears  <- noScaledYears
           conf$keyLogScale    <- keyLogScale
           conf$keyScaledYears <- (max(dat$years) - noScaledYears + 1):max(dat$years)
           conf$keyParScaledYA <- matrix(data = rep(c(0), 
                                                 each = noScaledYears * ncol(dat$propF)),
                                         nrow = noScaledYears)
           conf$constRecBreaks <- numeric(0)
         },
         `no misreporting` = {
           conf$constRecBreaks <- numeric(0)
         }
  )
  

  
  if (misreportingType == "rw") {
    par <- stockassessment2::defpar(dat, conf)
  } else {par <- stockassessment::defpar(dat, conf)}

  
  
  return(list(dat = dat, conf = conf, par = par))
}
