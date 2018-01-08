#### Extrapolate the A-matrix #######################################################

# load packages ---------------------------------------

require(devtools)
install_github('simonpedrobolivar/masterthesis/MRIOtools')
require(MRIOtools)
require(gdata)
require(data.table) # for handling large data sets
require(parallel)

# set constant parameters --------------------------------

n_ind       <- 35 # number of industries
n_countries <- 41 # number of countries (40 + 1 RoW)
years_obs   <- 1995:2011 # years for which IO table exists
years_new <- c(2012:2014, seq(2015, 2045, 10)) # year for which to make prediction
years <- c(years_obs, years_new)
n_years_obs     <- length(years_obs)
n_years <- length(years)
n_sectors   <- 4 # final demand sectors (household, non-gov, gov, capital formation)
exchange_rates <- c(1.36626,	1.30024	,1.12945	,1.11308	,1.06580	,0.92360	,0.89560	,0.94560	,1.13120,	1.24390,	1.24410	,1.25560,	1.37050	,1.47080,	1.39480,	1.32570,	1.39200) # 1 € = ...$US (according to WIOD)

# read names of countries and industries: 
config_sheet = read.xls("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/Calculation/WIOD_Config.xlsx", sheet = 3, header = T)
config_sheet2 = read.xls("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/Calculation/WIOD_Config.xlsx", sheet = 5, header = T)
countries <- as.character(config_sheet$Symbol)[1:41]
regions <- as.character(config_sheet$Region)[1:41]
industries <- as.character(config_sheet2$X.1)[-1]

industriesXcountries <- paste(rep(industries, n_countries), 
      as.vector(sapply(countries, 
                       FUN = function(x) return(rep(x, n_ind)), simplify = "vector")))

# loading the data --------------------------------------

setwd("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables")
A_list <- list() # 
for(i in 1:n_years_obs){
 # A_mat <- as.data.frame(read.table(paste("A_matrix",1994+i,".csv",sep = ""), 
 #                                   header = F, sep = " "))
  A_mat <- fread(paste("A_matrix",1994+i,".csv",sep = ""), 
                                     header = F, sep = " ")
  
  colnames(A_mat) <- rownames(A_mat) <- industriesXcountries
  A_list[[i]] <-  A_mat # append to list
  print(paste("year", i + 1994, "done!"))
}
names(A_list) <- years_obs
A_list[[1]][1:10, 1:10]

# prepare the data for extrapolation ------------------------------
# In order to fit a linear model (lm) for each coefficient of the Y-matrix following steps need to be undertaken: 
# 1) timeseries_obs for each coefficient (data frame with one column *years* and one col *coefficient value*)
# 2) fit a lm to each timeseries_obs (using the lm function)
# 3) predicting each coefficient into the future using the fitted models

# create timeseries_obs -------------------------------------------------------------------------------
timeseries_obs <- sapply(A_list, 
       FUN = function(x) return(apply(x, c(2,1), # c(2,1) damit reihenfolge erst zeile, dann spalte, wie in for-schleifen oben
                                      FUN = function(x) return(x))))
timeseries_obs <- as.data.table(timeseries_obs)
setnames(timeseries_obs, as.character(years_obs))
rownames(timeseries_obs) <- paste(as.vector(sapply(industriesXcountries, 
                                               FUN = function(x) return(rep(x, n_ind * n_countries)), 
                                               simplify = "vector")), 
                              "->", rep(industriesXcountries, n_ind * n_countries)) 

dim(timeseries_obs)
head(timeseries_obs)

timeseries_detailed <- as.data.table(matrix(ncol = ncol(timeseries_obs) + 4, nrow = nrow(timeseries_obs)))

timeseries_detailed[,5:(n_years_obs+4)] <- timeseries_obs

timeseries_detailed[,1] <- rep(as.vector(sapply(industries, 
                                            FUN = function(x) return(rep(x, n_ind * n_countries)))), n_countries)
timeseries_detailed[,2] <- as.vector(sapply(countries, 
                                            FUN = function(x) return(rep(x, n_ind * n_ind * n_countries))))
timeseries_detailed[,3] <- rep(industries, n_ind * n_countries * n_countries)
timeseries_detailed[,4] <- rep(as.vector(sapply(countries, 
                                            FUN = function(x) return(rep(x, n_ind)))), n_ind * n_countries)
setnames(timeseries_detailed, c("sending_industry", "sending_country", 
                                "receiving_industry", "receiving_country", as.character(years_obs))
)
#colnames(timeseries_detailed) <- c("sending_industry", "sending_country", 
 #                                  "receiving_industry", "receiving_country", as.character(years_obs))
head(timeseries_detailed)
save(timeseries_detailed, file = "/home/simon/Dokumente/Master/master_thesis/scripts/timeseries_detailed.RData")

# plot timeseries_obs
timeseries_detailed_DEU <- timeseries_detailed[sending_country == "DEU" & receiving_country == "DEU"]
par(mfrow = c(2,2))
for(i in 1:35){
  plot(years_obs, timeseries_DEU[i,5:(n_years_obs+4)], type = "l")
}

# check for trends ------------------------------------------------------------------------- 
timeseries_obs_DEU <- (timeseries_detailed_DEU[,-c(1:4)])
cor_vec_DEU <- apply(timeseries_obs_DEU, 1, FUN = function(x) return(cor(x, years_obs)))
cor_mat_DEU <- matrix(cor_vec_DEU, nrow = n_ind, ncol = n_ind, byrow = T)

cor_mat_DEU4plot <- t(cor_mat_DEU)
cor_mat_DEU4plot <- cor_mat_DEU4plot[,seq(from=ncol(cor_mat_DEU4plot),to=1,by=-1)] #reversing the columns
rgb.palette <- colorRampPalette(c("yellow","red"),space="rgb")


levelplot(data.matrix((cor_mat_DEU4plot)),aspect="iso", 
          xlab = "", ylab = "", 
          alternating = 2) #alternating=2 flips x axis labels up to top
cor_mat_DEU[7,]

#############################
# fit a model to timeseries_obs ------------------------------------------------------------------------------------
#############################
modelfitfun <- function(x){
  # x is a timeseries_obs
  fm <- glm(x ~ years_obs) # fit model
  preds_output <- predict(fm, newdata = data.frame("years_obs" = years_new), se.fit = F) # predictions
  setZeroFun <- function(x) ifelse(x < 0, return(0), return(x))
  preds_output <- apply(as.matrix(preds_output), c(1,2), FUN = setZeroFun(x)) # all values < 0 are set to 0
  return(preds_output)
}

# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores, type = "FORK")
system.time(
  timeseries_extrapolated <- as.data.table(t(parSapply(cl, 1:nrow(timeseries_obs), 
                                                       FUN = function(x) return(modelfitfun(as.numeric(timeseries_obs[x,]))))))
)
stopCluster(cl)


sum(timeseries_detailed[receiving_industry == "Agriculture, hunting, forestry and fishing" &
                    receiving_country == "DEU", "1995"])



setnames(timeseries_extrapolated, as.character(years_new)) # change col names
head(timeseries_extrapolated) # check resulting matrix

# create full timeseries with observed + extrapolated values: 
timeseries_full <- data.table(timeseries_detailed, timeseries_extrapolated, 
                              keep.rownames = T)
head(timeseries_full) # check resulting matrix

# plot results
timeseries_full_DEU <- timeseries_full[sending_country == "DEU" & receiving_country == "DEU"]
nrow(timeseries_full_DEU)
par(mfrow = c(2,2))
for(i in 1:35){
  plot(years, timeseries_full_DEU[i,5:(n_years+4)], type = "l", 
       main = paste(timeseries_full_DEU$sending_industry[i], "\n", "->",
                    timeseries_full_DEU$receiving_industry[i]), 
       cex.main = 0.7, 
       ylab = "")
}

sum(timeseries_full[receiving_industry == "Agriculture, hunting, forestry and fishing" &
                          receiving_country == "DEU", "1995"])

sum_inputs_DEU <- as.data.frame(matrix(0, ncol = n_years, nrow = n_ind))
setnames(sum_inputs_DEU, as.character(years))
for(i in 1:length(industries_realnames)){
  sum_inputs_DEU[i,] <- as.numeric(apply(timeseries_full[receiving_industry == industries_realnames[i]&
                          receiving_country == "DEU",-c(1:4)], 
        2, sum))   # summe von industrial input into sector "agriculture, ..."
}

plot(years, sum_inputs_DEU[1,], type = "l", ylim = c(0, 1))
legend("topleft", lty = 1, col = 1, 
       legend = industries[1], cex = 1)
for(i in 2:n_years){
  lines(years, sum_inputs_DEU[i,], col = i)
  legend("topleft", lty = 1, col = i, 
         legend = industries[i], cex = 1)
  Sys.sleep(3)
}


sum_inputs_DEU[which(min(sum_inputs_DEU[,1] - sum_inputs_DEU[,n_years]))]

min(sum_inputs_DEU[,n_years] - sum_inputs_DEU[,1])

# create new A matrices ---------------------------------------------------------------

Y_times_list <- list() # list for Y-matrixes for each year
Y_plus_se_times_list <- list()
Y_minus_se_times_list <- list()

Y_mat <- as.data.frame(matrix(0, nrow = length(timeseries_obs[[1]]), ncol = 6))
Y_plus_se_mat <- as.data.frame(matrix(0, nrow = length(timeseries_obs[[1]]), ncol = 6))
Y_minus_se_mat <- as.data.frame(matrix(0, nrow = length(timeseries_obs[[1]]), ncol = 6))
dim(Y_list[[1]][,c(4,3, 5:8)][,-c(1,2)])
# first for Observed values
for(i in 1:n_years_obs_obs){
  write.table(Y_list[[1]][,c(4,3, 5:8)][,-c(1,2)], paste("/home/simon/Dokumente/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables/Y_matrix", years_obs[i], ".csv", sep = ""), row.names = F, col.names = F, sep = ",") 
  Y_times_list[[i]] <- Y_list[[i]][,c(4,3, 5:8)]
}
# second for Extrapolated values
for(t in 1:length(years_new)){
  for(i in 1:n_sectors){ # for household, non-gov etc.
    for(j in 1:length(timeseries_obs[[1]])){ # for ind. x countries
      Y_mat[j,1] <- Y_plus_se_mat[j,1] <- Y_minus_se_mat[j,1] <- as.character(Y_list[[1]][j,4])
      Y_mat[j,2] <- Y_plus_se_mat[j,2] <- Y_minus_se_mat[j,2] <- as.character(Y_list[[1]][j,3])
      
      Y_mat[j,i+2] <- pred_list[[i]][[j]][t,2]
      Y_plus_se_mat[j,i+2] <- pred_list[[i]][[j]][t,2] + 2 * pred_list[[i]][[j]][t,3]
      Y_minus_se_mat[j,i+2] <- pred_list[[i]][[j]][t,2] - 2 * pred_list[[i]][[j]][t,3]
    }
  }
  colnames(Y_mat) <- c("index", "country", "household", "non_gov", "gov", "capital_formation")
  write.table(Y_mat[,-c(1,2)], paste("/home/simon/Dokumente/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables/Y_matrix", years_new[t], ".csv", sep = ""), row.names = F, col.names = F, sep = ",") 
  
  write.table(Y_plus_se_mat[,-c(1,2)], paste("/home/simon/Dokumente/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables/Y_plus_se_matrix", years_new[t], ".csv", sep = ""), row.names = F, col.names = F, sep = ",") 
  
  write.table(Y_minus_se_mat[,-c(1,2)], paste("/home/simon/Dokumente/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables/Y_minus_se_matrix", years_new[t], ".csv", sep = ""), row.names = F, col.names = F, sep = ",") 
  Y_times_list[[length(Y_times_list)+1]] <- Y_mat
  Y_plus_se_times_list[[length(Y_plus_se_times_list)+1]] <- Y_plus_se_mat
  Y_minus_se_times_list[[length(Y_minus_se_times_list)+1]] <- Y_minus_se_mat
}
names(Y_times_list) <- names(Y_plus_se_times_list) <- names(Y_minus_se_times_list) <-years_new







