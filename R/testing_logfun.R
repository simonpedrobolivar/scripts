require(nlmrt)
require(gtools)

timeseries_detailed_DEU
logfun <- function(C, k, t, t0) return(C / (1 + exp( -k * (t-t0) ))) 
modelfitfun <- function(x){
  # x is a timeseries_obs
  #fm <- glm(x ~ years_obs) # fit model
  par_est <- coef(lm(logit(x/(2*max(x)))~years_obs)) # first estimate of the parameters
  fm <- nlxb(x ~ par_C / (1 + exp( -par_k * (years_obs - par_t0) ) ), 
             start = list(par_k = as.numeric(par_est[1]), par_C = 2*max(x), par_t0 = as.numeric(par_est[2])), 
             data = list(x = x, years_obs = years_obs))
  #preds_output <- predict(fm, newdata = data.frame("years_obs" = years_new), se.fit = F) # predictions
  preds_output <- logfun(t = years, C = fm$coefficients[1], k = fm$coefficients[2], t0 = fm$coefficients[3])
  setZeroFun <- function(x) ifelse(x < 0, return(0), return(x))
  preds_output <- apply(as.matrix(preds_output), c(1,2), FUN = setZeroFun) # all values < 0 are set to 0
  return(preds_output)
}


testpreds <- logfun(k = as.numeric(par_est[1]), C = 2*max(x), t0 = as.numeric(par_est[2]) + 1995, t = years_obs)

par(mfrow = c(1,1))
plot(years, c(x, rep(NA, 7)), type = "l", ylim = c(0, max(testpreds)))
lines(years, testpreds, col = "red")

# Calculate the number of cores
no_cores <- 12
# Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")
system.time(
  timeseries_extrapolated_DEU <- as.data.table(t(parSapply(cl, 1:nrow(timeseries_detailed_DEU), 
                                                       FUN = function(x) return(modelfitfun(as.numeric(timeseries_detailed_DEU[x,-c(1:4)]))))))
)
stopCluster(cl) # 1416 sec = 23 min

setnames(timeseries_extrapolated_DEU, as.character(years)) # change col names
head(timeseries_extrapolated_DEU) # check resulting matrix

# create full timeseries with observed + extrapolated values: 
timeseries_extrapolated_full_DEU <- data.table(timeseries_detailed_DEU[,1:4], timeseries_extrapolated_DEU, 
                              keep.rownames = T)
head(timeseries_extrapolated_full_DEU) # check resulting matrix



dif_obs_est <- timeseries_extrapolated_DEU[,1:length(years_obs)] - timeseries_detailed_DEU[,-c(1:4)]

max(abs(dif_obs_est))
min(abs(dif_obs_est))

par(mfrow = c(4,4))
for(i in 33:48){
  plot(years, c(timeseries_detailed_DEU[i,-(1:4)], rep(NA, 7)), type = "l", 
       ylim = c(0, max(timeseries_extrapolated_DEU[i,])), 
       main = paste(timeseries_full_DEU$sending_industry[i], "\n", "->",
                    timeseries_full_DEU$receiving_industry[i]), 
       cex.main = 0.7, 
       ylab = "")
  lines(years, timeseries_extrapolated_DEU[i,], col = "red")
}



