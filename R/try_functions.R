#### Extrapolate the A-matrix #######################################################

# load packages ---------------------------------------

require(devtools)
install_github('simonpedrobolivar/masterthesis/MRIOtools')
require(MRIOtools)
require(gdata)
require(data.table) # for handling large data sets
require(parallel)
require(lattice)

A_list[[1]][1:10, 1:5]

system.time(
A_extrapolated_list <- extrapolate_matrix(A_list, 
                                          years.obs = years_obs, 
                                          years.est = years_new, 
                                          parallel = F)
)


#####################################################################
##### test the extrapolate_matrix function
#####################################################################

# create timeseries of matrices
years_obs_test <- 2000:2010
years_est_test <- c(2015, 2020)
test_mat_list <- list()
for(i in 1:length(years_obs_test)){
  test_mat_list[[i]] <- matrix(rnorm(25, mean = i), nrow = 5, ncol = 5)
}

# extrapolate
extrapolate_matrix(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test, parallel = T, n.cores = 5)




