# Demonstration of how to calculate the time series mean of an
# exponentiated random walk.
library(dplyr)
library(tidyr)
library(ggplot2)
source("inits_fns.R")

Nrep = 1000000 # 1 million replicate simulations

# Choose the time series mean and variance you want (on exp scale)
tsmean = 5.5
tsvar  = 6.0



# Parameters that affect the exponentiated time series mean:
T = 10 # number of time steps
mu_c = 1 # mean initial value
sd_c = 0.3 # sd of initial value
mu_e = 0 # mean of error (drift parameter)
sd_e = 0.2 # sd of error


# Generate random walk on log-scale
x = matrix(data = NA, nrow = T, ncol = Nrep)
x[1,] = matrix(rnorm(n = Nrep, mean = mu_c, sd = sd_c), nrow = 1)
for (t in 2:T) {
  x[t,] = x[t-1,] + matrix(rnorm(n = Nrep, mean = mu_e, sd = sd_e), nrow = 1)
}

# Calculate summary stats at each time step for logged and exp time series
dat <-
  data.frame(t = 1:T, 
             x = rowMeans(x), 
             var_x = apply(x, 1, var),
             range_x = apply(x, 1, sd),
             expx = rowMeans(exp(x)),
             var_expx = apply(exp(x), 1, var),
             range_expx = apply(exp(x), 1, function(x) max(x) - min(x))
             ) %>%
  gather(variable, value, -t)
  
# Plot
ggplot(data = dat, aes(x = t, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme_bw() +
  ylab("value at time t")

# Time series mean empirically and analytically
(analyticalMean <- mean(exp(mu_c + sd_c^2/2)*exp((mu_e + sd_e^2/2) * (1:T-1))))
(empiricalMean  <- mean(dat$value[dat$variable == "expx"]))


# Variance at time t empirically and analytically
Tm1 = T-1 # max time minus 1
E_c = exp(mu_c + 0.5* sd_c^2) # Expected value of initial condition
E_e = exp((1:Tm1) * (mu_e + 0.5 * sd_e^2)) # Expected value of error
Var_c = exp(2*mu_c + sd_c^2) * (exp(sd_c^2) - 1) # Variance of initial condition
Var_e = exp((1:Tm1)*(2*mu_e + 2*sd_e^2)) - exp(2 * (1:Tm1)*(mu_e + 0.5 * sd_e^2)) # Variance of error
(analyticalVarAtT <- c(Var_c, E_c^2 * Var_e + E_e^2 * Var_c + Var_c * Var_e)) # Variance of two independent variables
(empiricalVarAtT <- dat$value[dat$variable == "var_expx"])



# Time series mean variance empirically and analytically
(mean(analyticalVarAtT))
(mean(empiricalVarAtT))

#### DEMONSTRATE OPTIMIZER ####
# Given a target timseries mean and variance, and
# a time series length T, this function will tell you
# which values of mu_c and sd_c (initial condtions of random walk)
# get you closest to those targets. Assumes mu_e = 0, and 
# sd_e = sd_c (as in main simulation code).
(opt_par <-optim(par = c(2, 0.4), # starting par[1] = mu_c, par[2] = sd_c
                 fn = calc_obj, 
                 tsmean_target = 5.5, tsvar_target = 6.0, T = T))

# Check ts mean with optimal intital conditions
calc_tsmean(mu_c = opt_par$par[1], sd_c = opt_par$par[2],
            mu_e = 0, sd_e = opt_par$par[2], T = T)

# Check ts var with optimal intital conditions
calc_tsvar(mu_c = opt_par$par[1], sd_c = opt_par$par[2],
           mu_e = 0, sd_e = opt_par$par[2], T = T)

