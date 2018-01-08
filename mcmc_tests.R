library(R2jags)


fm_jags <- function(){
  # priors
  #par_k ~ dunif(-1, 1)
  #par_C ~ dunif(0.00001, 0.9999)
  #par_t0 ~ dunif(0, 20)
  #tau.obs ~ 
  sigma ~ dgamma(1.0E-3, 1.0E-3)
  phi1 ~  dunif(0, 1)
  phi2 ~  dunif(1995, 2011)
  phi3 ~  dnorm(0, 1.0E-4)
  #h2 ~ dnorm(0, 0.1)
  # state process
  for (t in 1:(T-1)){
    #logcoeff.est[t+1] <- logcoeff.est[t] + log(max(0.001, 
     #                                      (1 + par_k # growth rate
      #                                      * (1 - (logcoeff.est[t] / par_C)) # carrying capacity
       #                                     ))) # additional terms
    Y[t] ~ dnorm(eta[t], sigma)
    eta[t] <- phi1 / (1 + exp(-(x[t]-phi2)/phi3))
    #coeff.est[t] ~ dnorm(par_C / (1 + exp(-par_k * (t - par_t0))), sigma) 
  }
  #phi1[i] ~  dnorm(mu1, tau1)
  #for (t in 1:T){
  #  y[t] ~ dnorm(coeff.est[t], tau.obs)
  #}
  
  # pop size on real scale
  #for (t in 1:T){
  #  coeff.est[t] <- exp(logcoeff.est[t])
  #}
}

# bundle data
test_data <- timeseries[[1]][[1]]
#bugs.data <- list(y = N.obs, coy = Coy.obs, clim = min_temp_w.obs, hunt = Huntprev.obs,clim2 = min_temp_w.obs_quadrat, hunt2 = Huntprev.obs_quadrat, coy2 = Coy.obs_quadrat, T = length(N.obs))
bugs.data <- list(Y = as.numeric(test_data[1,]), T = nrow(test_data), x = years_obs)

# initial values
inits <- function(){
  list(#par_k = runif(1,-1,1), par_C = runif(1, 0.01, 0.9), par_t0 = runif(1, 0, 20), 
       sigma = 0.1, 
       phi1=0.1, phi2=2000, phi3=0.1
       )
}#coeff.est = c(runif(1, 0, 1), rep(NA, (length(test_data[1,])-1)))

# pars monitored
parameters <- c("phi1", "phi2", "phi3", "sigma")
  #c("par_k", "par_C", "par_t0", "sigma")

# mcmc settings
ni <- 30
nt <- 3
nb <- 15
nc <- 3


# call winbugs
fm1 <- jags(bugs.data, inits, parameters.to.save = parameters, model.file = fm_jags,
               n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = 100, DIC = T)



########################################
##### trying with test data from jags-package
########################################


model.file <- system.file(package="R2jags", "model", "schools.txt")
# Let's take a look:
file.show(model.file)

schoolsmodel <- function() {
  for (j in 1:J){                     # J=8, the number of schools
    y[j] ~ dnorm (theta[j], tau.y[j]) # data model:  the likelihood
    tau.y[j] <- pow(10, -2)        # tau = 1/sigma^2
    y[j] <- 
  }
  for (j in 1:J){
    theta[j] ~ dnorm (mu, tau)        # hierarchical model for theta
  }
  tau <- pow(sigma, -2)               # tau = 1/sigma^2
  mu ~ dnorm (0.0, 1.0E-6)            # noninformative prior on mu
  sigma ~ dunif (0, 1000)             # noninformative prior on sigma
}


# data
J <- 17
y <- as.numeric(test_data[6,])
#sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)


jags.data <- list("y","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){
  list("mu"=0.1,"sigma"=0,"theta"=0.1)
}

## You can input data in 4 ways
## 1) data as list of character
jagsfit <- jags(data=list("y","J"), inits=jags.inits, jags.params,
                n.iter=10, model.file=schoolsmodel)

## 2) data as character vector of names
jagsfit <- jags(data=c("y","sd","J"), inits=jags.inits, jags.params,
                n.iter=10, model.file=model.file)

## 3) data as named list
jagsfit <- jags(data=list(y=y,sd=sd,J=J), inits=jags.inits, jags.params,
                n.iter=10, model.file=model.file)


plot(jagsfit)




