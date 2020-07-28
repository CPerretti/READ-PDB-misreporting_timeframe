
# Functions to calculate the mean and sd of the initial
# condition of the random walk.

# Calculate time series mean
rwTsMean <- function(mu_c, sd_c, mu_e, sd_e, T) {
  mean(exp(mu_c + sd_c^2/2)*exp((mu_e + sd_e^2/2) * (1:T-1)))
}

# Calculate time series variance
rwTsVar <- function(mu_c, sd_c, mu_e, sd_e, T) {
  Tm1 = T-1 # max time minus 1
  E_c = exp(mu_c + 0.5* sd_c^2) # Expected value of initial condition
  E_e = exp((1:Tm1) * (mu_e + 0.5 * sd_e^2)) # Expected value of error
  Var_c = exp(2*mu_c + sd_c^2) * (exp(sd_c^2) - 1) # Variance of initial condition
  Var_e = exp((1:Tm1)*(2*mu_e + 2*sd_e^2)) - exp(2 * (1:Tm1)*(mu_e + 0.5 * sd_e^2)) # Variance of error
  
  mean(c(Var_c, E_c^2 * Var_e + E_e^2 * Var_c + Var_c * Var_e))
}

# Objective function for initis to be minimized 
initsObj <- function(par, tsmean_target, tsvar_target, T) {
  mu_c <- exp(par[1]) #bounded [0, Inf]
  sd_c <- 0.001 + exp(par[2]) # bounded [0.001, Inf]
  ts_mean <- rwTsMean(mu_c, sd_c, mu_e = 0, sd_e = sd_c, T)
  ts_var  <- rwTsVar(mu_c, sd_c, mu_e = 0, sd_e = sd_c, T)
  
  obj <- sqrt((tsmean_target - ts_mean)^2 + (tsvar_target - ts_var)^2)
  return(obj)
}

