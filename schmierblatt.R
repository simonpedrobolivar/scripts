#### Schmierblatt

# test function to create timeseries: 
test_list <- list()
for(i in 1:10){
  test_list[[i]] <- matrix(runif(9), ncol = 3, nrow = 3)
}
test <- sapply(test_list, 
               FUN = function(x) return(apply(x, c(2,1), FUN = function(x) return(x))))

timeseries_test <- list()
for(i in 1:3){
  for(j in 1:3){
    timeseries_test[[length(timeseries_test)+1]] <- sapply(test_list, 
                                                           FUN = function(x) return(x[i,j])) 
  }
}
for(i in 1:9) print(timeseries_test[[i]] - test[i,])

# creating timeseries with for-loops
timeseries_names <- paste(industriesXcountries[i], "->", industriesXcountries[j])
for(i in 1:(n_ind*n_countries)){
  for(j in 1:(n_ind*n_countries)){
    timeseries[[length(timeseries)+1]] <- sapply(A_list, 
                                                 FUN = function(x) return(x[i,j])) # get timeseries of coefficient in row i and col j 
    names(timeseries)[[length(timeseries)]] <- timeseries_names
    #print(paste("i =", i, "j =", j)) # very slow
  }
  print(paste("i =", i))
}

