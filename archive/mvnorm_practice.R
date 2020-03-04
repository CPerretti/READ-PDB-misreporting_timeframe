library(mvtnorm)
library(TMB)
library(dplyr)

set.seed(543)

nVar <- 4
nObs <- 10000
logSdS <- log(rep(1, nVar)) # sd = 1
rho <- 0.7 # cor between adjacent ages = 0.7

# Set up correlation matrix
scor <- diag(nVar)
for(i in 1:nVar){
  for(j in 1:nVar){
    if (i == j) {
    } else {
      scor[i,j] = rho ^ abs(i-j) # ar1 decay of correlation
      scor[j,i] = scor[i,j] # symmetric matrix
    }
  }
}

# Convert cor matrix to cov
sigma <- matrix(NA, nrow = nVar, ncol = nVar)
for (i in 1:nVar){
  for (j in 1:nVar){
    sigma[i,j] = exp(logSdS[i]) * exp(logSdS[j]) * scor[i,j]
  }
}

# Generate MVN error using noncentered parameterization. This
# allows you to generate MVN error with correlation by simply
# generating standard normals and tranforming them.
L <-  t(chol(sigma))
res <- L %*% matrix(rnorm(n = nVar * nObs), nrow = nVar, ncol = nObs)

# simulate random walk
x <- matrix(nrow = nVar, ncol = nObs)
x[,1] <- res[,1]
for (i in 2:nObs) {
  
  x[,i] <- x[,i-1] + res[,i]  
  
}

cor(diff(t(x))) # check that cor is what you expect

# Fit using either the noncentered version of MVN or
# the built-in TMB vesion of MVN.
compile("fitMvnorm_noncentered.cpp")
#compile("fitMvnorm.cpp")

dyn.load(dynlib("fitMvnorm_noncentered"))
#dyn.load(dynlib("fitMvnorm"))

parameters <- list(logSdS = rep(0, nVar),
                   itrans_rho = 0.1)

obj <- MakeADFun(data = list(x = x,
                             nVar = nVar), 
                 parameters,
                 DLL = "fitMvnorm_noncentered",
                 #DLL = "fitMvnorm"
                 )

opt <- do.call("optim", obj)

opt

out <- sdreport(obj)

# Check that you get what you should get
data.frame(variable = names(out$value),
           value    = out$value,
           sd       = out$sd) %>%
  filter(variable %in% c("scor", "ssd", "rho"))

scor

obj$report()



