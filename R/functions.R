# functions


#' aggregates data.tables with two columns: 1st col Country/Industry whatever, 2nd col "V1" with values. 
#' First, the data.table is sorted based on the values of column "V1". Then, all but n rows are aggregated to one sector "Rest". 
#'
#' @param dt data.table with two columns: 1st col Country/Industry whatever, 2nd col "V1" with values
#' @param n number of sectors/countries/etc. that stay
#' @return a aggregated dtrix
#' @export


aggregate_datatable <- function(dt,
                                n, # number of sectors/countries/etc. that stay
                                col # name of column used for aggregating
){
  # aggregates a dtrix
  # first n sectors/countries/etc. stay, all others are subsumed into one sector "Other"
  setorderv(dt, cols = col, order = -1)
  dt_agg <- dt[1:n,]
  tempdt <- data.table("Rest", dt[(n+1):nrow(dt), sum(V1)])
  setnames(tempdt, names(dt))
  dt_agg <- rbind(dt_agg, tempdt)
  return(dt_agg)
}



#' aggregates matrices.
#'
#' @param mat matrix with years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
#' @param n number of sectors/countries/etc. that stay
#' @param fun function,  determines how first n sectors/countries/etc are determined
#' @return a aggregated matrix
#' @export




aggregate_matrix <- function(mat,
                             n, # number of sectors/countries/etc. that stay
                             fun = mean # determines how first n sectors/countries/etc are determined
){
  # aggregates a matrix
  # first n sectors/countries/etc. stay, all others are subsumed into one sector "Other"
  new_mat <- matrix(0, ncol = n+1, nrow = nrow(mat))
  names_agg <- names(sort(apply(mat, 2, fun), decreasing = T))[1:n]
  index_vec <- vector(mode = "numeric", length = n)
  for(i in 1:n){
    new_mat[,i] <- mat[,which(colnames(mat) == names_agg[i])]
    index_vec[i] <- which(colnames(mat) == names_agg[i])
  }
  new_mat[,n+1] <- rowSums(mat[,-index_vec] )
  rownames(new_mat) <- rownames(mat)
  colnames(new_mat) <- c(colnames(mat)[index_vec], "Other")
  return(new_mat)
}


#' creates a data.table which contains the timeseries of each coefficient of a timeseries of matrices
#'
#' @param mat.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t. 
#' @export

create_timeseries <- function(mat.list){
  require(data.table)
  ts <- as.data.table(sapply(mat.list, 
                             FUN = function(x) return(apply(x, c(2,1), # c(2,1) damit reihenfolge erst zeile, dann spalte, wie in for-schleifen oben
                                                            FUN = function(x) return(x)))))
  return(ts)
}




#' colwise cumulate values of a matrix (required for stacked_area_plot)
#'
#' @param mat matrix
#' @param order boolean
#' @return a cumulated matrix
#' @export

cumulate_matrix <- function(mat, order = T){
  # colwise cumulate values of a matrix (required for stacked_area_plot)
  if(order == T){
    if(length(which(colnames(mat) == "Other")) == 1){ # sector "Other" as first col --> better for plotting
      new_mat <- mat[,-which(colnames(mat) == "Other")]
      new_mat <- new_mat[,order(colMeans(new_mat))]
      new_mat <- cbind("Other" = mat[,which(colnames(mat) == "Other")], new_mat)
    }else new_mat <- mat[,order(colMeans(mat))]
  }else{new_mat <- mat}
  for(i in 2:ncol(new_mat)){
    new_mat[,i] <- new_mat[,i-1] + new_mat[,i]
  }
  return(new_mat)
}


#' makes a cross-validation of a time-series. The time-series is split at a given year into a training data set and a train data set. A model is fitted to the training data and forecasts are made to the remaining years. These forecasts are compared to the "real" values.
#'
#' @param x a timeseries of length(years.obs) observations
#' @param years.obs years for which observations exist
#' @param years.split first year, where time-series should be splitted to training and train data set. year.split is the last year of the first training data set.
#' @export

cv <- function(x, years.obs, year.split, type = "glm", ... ){
  # x is a timeseries.obs
  preds.dt <- data.table::as.data.table(matrix(ncol = 4, nrow = 0))
  setnames(preds.dt, c("Year_split", "Year_forecast", "est", "obs"))
  n.years <- length(years.obs)
  n.years2model <- years.obs[length(years.obs)] - year.split
  for(i in 1:n.years2model){
    ts.train <- x[1:(n.years - n.years2model - 1 + i)] # train data
    years.train <- years.obs[1:(n.years - n.years2model - 1 + i)] # train years
    # fit model
    if(type %in% c("glm", "glm2", "glm3")){
      if(type == "glm")  fm <- glm(ts.train ~ years.train)
      if(type == "glm2") fm <- glm(ts.train ~ years.train + I(years.train^2))
      if(type == "glm3") fm <- glm(ts.train ~ years.train + I(years.train^2) + I(years.train^3))
      preds <- predict(fm, newdata = data.frame("years.train" = years.obs[(n.years - n.years2model + i):n.years]))
    }
    if(type == "gam1"){
      # TODO. not running yet
      if(sum(ts.train) == 0){
        preds <- rep(0, length(years.obs[(n.years - n.years2model + i):n.years]))
      }else{
        k <- length(ts.train) - 1
        data <- data.frame("ts.train" = ts.train, "years.train" = years.train)
        fm    <- gamm(ts.train ~ s(years.train, bs = "cr", k = k), data = data,
                      correlation = corARMA(form = ~ 1|years.train, p = 1, q = 1))
        preds <- as.numeric(predict(fm$gam,
                                    newdata = data.frame("years.train" = years.obs[(n.years - n.years2model + i):n.years])))
      }
    }
    if(type == "ets"){
      fm <- ets(ts(ts.train,
                   start = years.train[1],
                   end = years.train[length(years.train)]), ...)
      preds <- as.numeric(forecast(fm,
                                   h = years.obs[n.years] - years.train[length(years.train)])$mean) # last year - last year of train data
      #print(paste("h=",years.obs[n.years] - years.train[length(years.train)]))
    }
    preds.dt <- rbindlist(list(preds.dt, list(rep(year.split + i - 1, n.years2model + 1 - i), (year.split + i):years.obs[length(years.obs)], preds, x[(n.years - n.years2model + i):n.years])))
    
  }
  
  return(preds.dt)
}



#' Returns a list containing the extrapolated matrices.
#'
#' @param col.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.est years for which estimations/extrapolations should be done
#' @param parallel boolean. calculation parallel on multiple cores
#' @param n.cores number of cores. Default n.cores = 1.
#' @param save.output boolean. Shall the output (new matrices) be saved as .csv to directory dir (specified with param dir)?
#' @param dir directory to where the new matrices should be saved
#' @export

extrapolate_A_column <- function(col.list, years.obs, years.est, scaleTo1 = F,parallel = F, n.cores = 1,
                                 save.output = F, dir){
  
  n.row <- nrow(col.list[[1]])
  if(scaleTo1) col.sum.orig <- 0
  # save sum of each column of the latest A-matrix (later used for rescaling the extrapolated matrices)
  else col.sum.orig <- as.numeric(sum(col.list[[length(col.list)]]))
  # create timeseries, each col represents one year
  ts <- create_timeseries(col.list)
  
  if(parallel == T){ # parallele compution on multiple cores
    # Initiate cluster
    cl <- parallel::makeCluster(n.cores, type = "FORK")
    ts.extrapolated <- as.data.table(t(parSapply(cl, 1:nrow(ts),
                                                 FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                   years.obs = years.obs,
                                                                                   years.est = years.est)))))
    stopCluster(cl)
  }else{ # computation only on one single core
    ts.extrapolated <- as.data.table(t(sapply(1:nrow(ts),
                                              FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                years.obs = years.obs,
                                                                                years.est = years.est)))))
  }
  # create new matrices
  vec2mat <- function(x){
    # converts vector into matrix (size: n.row X n.col)
    return(as.data.table(matrix(x, nrow = n.row,
                                ncol = 1,
                                byrow = T))) # row-wise
  }
  # apply vec2mat for all columns (=years) of ts.extrapolated
  mat.extrapolated.list <- apply(ts.extrapolated, 2, FUN = vec2mat)
  names(mat.extrapolated.list) <- years.est
  # scale matrices
  col.sums.new_list <- lapply(mat.extrapolated.list, sum)
  
  for(i in 1:length(mat.extrapolated.list)){
    #each element of column i is multiplicated with col.sum.orig(i)/col.sums.new(i)
    if(col.sums.new_list[[i]] > 0) mat.extrapolated.list[[i]] <- mat.extrapolated.list[[i]] * (col.sum.orig/col.sums.new_list[[i]]) # to avoid division by zero
  }
  if(save.output == T){
    # TODO
  }
  return(mat.extrapolated.list)
}


#' ?
#'
#' @param x a data.table with timeseries of all coefficients of a matrix. (Rowwise, i.e. 1st element if (1,1), 2nd is (1,2), etc. )
#' @param n.row number of rows of the output matrix
#' @param n.col number of cols of the output matrix
#' @export



vec2mat <- function(x, n.row, n.col){
  # converts vector into matrix (size: n.row X n.col)
  return(as.data.table(matrix(x, nrow = n.row,
                              ncol = n.col,
                              byrow = T))) # row-wise
}

#' .
#'
#' @param mat_list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.split first year, where time-series should be splitted to training and test data set. year.split is the last year of the first training data set. 
#' @param scale should each timeseries be scaled??
#' @param parallel boolean. calculating parallelly on multiple cores?
#' @param n.cores number of cores. Default n.cores = 1.
#' @export

validate_forecast <- function(mat.list, years.obs, year.split, parallel = F, n.cores = 1,
                              type = "glm", scale = F, progress.bar = F){
  n.col <- ncol(mat.list[[1]])
  n.row <- nrow(mat.list[[1]])
  # create timeseries
  ts <- create_timeseries(mat.list)
  if(scale){ # scale each timeseries (center to 0, scaled by root-mean-squatered). It is necessary to transpose ts first, then scale and then transpose again, because data.tables can (as far as I know) only apply functions column-wise. Time series with only Zeros (0) are scaled to NaN and thus need to be set to 0 again. 
    #ts <- (transpose(transpose(ts)[ , lapply(.SD, scale)]))[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))]
    ts <- apply(transpose(as.data.table(apply(ts, 1, scale))), c(1,2), function(x) ifelse(is.nan(x), 0, x))
    
  }
  if(!progress.bar){
    if(parallel == T){ # parallele compution on multiple cores
      # Initiate cluster
      cl <- parallel::makeCluster(n.cores, type = "FORK")
      clusterCall(cl = cl, fun = library(mgcv))
      preds_dt <- rbindlist(parLapply(cl, 1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
      stopCluster(cl)
    }else{ # computation only on one single core
      preds_dt <-  rbindlist(lapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
    }
  }else{ # show progress bar
    if(parallel == T){ # parallele compution on multiple cores
      # Initiate cluster
      cl <- parallel::makeCluster(n.cores, type = "FORK")
      preds_dt <- rbindlist(pblapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type), cl = cl), idcol = T)
      stopCluster(cl)
    }else{ # computation only on one single core
      preds_dt <-  rbindlist(pblapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
    }
  }
  
  return(preds_dt)
}

#validate_forecast(Y_obs_list, years_obs, 2000, parallel = T, n.cores = 3,
                              type = "gam1", scale = T, progress.bar = T)

#cv(rep(0, 17), 1:17, 6, type = "gam1")


#' Returns a list containing the extrapolated matrices.
#'
#' @param mat.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.est years for which estimations/extrapolations should be done
#' @param parallel boolean. calculation parallel on multiple cores
#' @param n.cores number of cores. Default n.cores = 1.
#' @param save.output boolean. Shall the output (new matrices) be saved as .csv to directory dir (specified with param dir)?
#' @param dir directory to where the new matrices should be saved
#' @export

extrapolate_A_mat <- function(mat.list, years.obs, years.est, scaleTo1 = F,parallel = F, n.cores = 1,
                              save.output = F, dir){
  n.col <- ncol(mat.list[[1]])
  n.row <- nrow(mat.list[[1]])
  if(scaleTo1) col.sums.orig <- vector(mode = "numeric", length = n.col)
  # save sum of each column of the latest A-matrix (later used for rescaling the extrapolated matrices)
  else col.sums.orig <- as.numeric(colSums(mat.list[[length(mat.list)]]))#col.sums.orig <- lapply(mat.list, colSums)
  # create timeseries
  ts <- create_timeseries(mat.list)
  
  if(parallel == T){ # parallele compution on multiple cores
    # Initiate cluster
    cl <- parallel::makeCluster(n.cores, type = "FORK")
    ts.extrapolated <- as.data.table(t(parSapply(cl, 1:nrow(ts),
                                                 FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                   years.obs = years.obs,
                                                                                   years.est = years.est)))))
    stopCluster(cl)
  }else{ # computation only on one single core
    ts.extrapolated <- as.data.table(t(sapply(1:nrow(ts),
                                              FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                years.obs = years.obs,
                                                                                years.est = years.est)))))
  }
  # create new matrices
  vec2mat <- function(x){
    # converts vector into matrix (size: n.row X n.col)
    return(as.data.table(matrix(x, nrow = n.row,
                                ncol = n.col,
                                byrow = T))) # row-wise
  }
  # apply vec2mat for all columns (=years) of ts.extrapolated
  mat.extrapolated.list <- apply(ts.extrapolated, 2, FUN = vec2mat)
  names(mat.extrapolated.list) <- years.est
  # scale matrices
  col.sums.new_list <- lapply(mat.extrapolated.list, colSums)
  #rescale <- function(x, col.sums.new){
  # x is one matrix A
  #
  #   p <- as.numeric(col.sums.orig/col.sums.new)
  #  return(as.matrix(x) %*% diag(p))
  # }
  #p <- as.numeric(col.sums.orig/col.sums.new_list[[1]])
  #colSums(as.matrix(mat.extrapolated.list[[1]]) %*% diag(p)) - col.sums.orig
  for(i in 1:length(mat.extrapolated.list)){
    #each element of column i is multiplicated with col.sums.orig(i)/col.sums.new(i)
    mat.extrapolated.list[[i]] <- as.data.table(as.matrix(mat.extrapolated.list[[i]]) %*% diag(as.numeric(col.sums.orig/col.sums.new_list[[i]])))
  }
  if(save.output == T){
    # TODO
  }
  mat.extrapolated.list <- lapply(mat.extrapolated.list, 
                                  function(x) apply(x, c(1,2), 
                                                    function(x) ifelse(is.nan(x), 0, x))) # all NaN values (resulting from matrix multipliakation) are set to 0
  return(mat.extrapolated.list)
}



#' Returns a list containing the extrapolated matrices.
#'
#' @param mat_list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.est years for which estimations/extrapolations should be done
#' @param parallel boolean. calculation parallel on multiple cores
#' @param n.cores number of cores. Default n.cores = 1.
#' @param save.output boolean. Shall the output (new matrices) be saved as .csv to directory dir (specified with param dir)?
#' @param dir directory to where the new matrices should be saved
#' @export

extrapolate_matrix <- function(mat.list, years.obs, years.est, parallel = F, n.cores = 1,
                               save.output = F, dir){
  n.col <- ncol(mat.list[[1]])
  n.row <- nrow(mat.list[[1]])
  # create timeseries
  ts <- create_timeseries(mat.list)
  
  if(parallel == T){ # parallele compution on multiple cores
    # Initiate cluster
    cl <- parallel::makeCluster(n.cores, type = "FORK")#
    #doParallel::registerDoParallel(cl)
    #ts.extrapolated <- foreach::foreach(i = 1:n.row, .combine = rbind)%dopar%{
    # .packages = c('data.table'), .combine = data.table::rbind
    #  res.vec <- matrix(nrow = length(years.est), ncol = n.col)
    #  for(j in 1:n.col){
    #    ts <- sapply(mat.list,"[", i, j)
    #    res.vec[j] <- fitmodel(as.numeric(ts),
    #                           years.obs = years.obs,
    #                            years.est = years.est)
    #  }
    #  res.vec
    #}
    
    
    ts.extrapolated <- as.data.table(t(parSapply(cl, 1:nrow(ts),
                                                 FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                   years.obs = years.obs,
                                                                                   years.est = years.est)))))
    stopCluster(cl)
  }else{ # computation only on one single core
    ts.extrapolated <- as.data.table(t(sapply(1:nrow(ts),
                                              FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                years.obs = years.obs,
                                                                                years.est = years.est)))))
  }
  # create new matrices
  vec2mat <- function(x){
    # converts vector into matrix (size: n.row X n.col)
    return(as.data.table(matrix(x, nrow = n.row,
                                ncol = n.col,
                                byrow = T))) # row-wise
  }
  # apply vec2mat for all columns (=years) of ts.extrapolated
  mat.extrapolated.list <- apply(ts.extrapolated, 2, FUN = vec2mat)
  names(mat.extrapolated.list) <- years.est
  if(save.output == T){
    # TODO
  }
  return(mat.extrapolated.list)
}




#' fits a model to a timeseries and extrapolates it into the future
#'
#' @param x a timeseries of length(years.obs) observations
#' @param years.obs years for which observations exist
#' @param years.est years for which estimations/extrapolations should be done
#' @export

fitmodel <- function(x, years.obs, years.est, type = "glm"){
  # x is a timeseries_obs
  
  if(type == "glm"){
    fm <- glm(x ~ years.obs) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  if(type == "glm2"){
    fm <- glm(x ~ years.obs + I(years.obs^2)) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  if(type == "glm3"){
    fm <- glm(x ~ years.obs + I(years.obs^2) + I(years.obs^3)) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  
  if(type == "gam"){
    fm <- gam(x ~ s(years.obs))
  }
  
  
  return(preds_output)
}




#' function to get detailed matrix for ONE country.
#'
#' @param mat matrix
#' @param country.name character, name of the country
#' @param time.vec vector
#' @return a matrix
#' @export

getMatrix4Country <- function(mat, country.name, time.vec){
  #function to get detailed matrix for ONE country
  mat_country <- as.data.frame(matrix(nrow = length(time.vec), ncol = nrow(mat[[1]])))
  colnames(mat_country) <- rownames(mat[[1]])
  rownames(mat_country) <- time.vec
  for(i in 1:length(time.vec)){
    mat_country[i,] <- mat[[i]][,which(colnames(mat[[1]]) == as.character(country.name))]
  }
  return(mat_country)
}



#' plots the time series of one cell/coefficient of a list that contains time series of matrices (such as SUT, Y, etc.).
#'
#' @param mat_list a list containing a timeseries of matrices/2D-df/dt. Each element of the list represents on matrix of year t.
#' @param row.index the row index of the cell
#' @param col.index the col.index of the cell
#' @param years A vector with the years for which data exists. Needs to be equal to the length of @param mat_list
#' @param ... further arguments for plot(). See also ?plot
#' @export

plot_timeseries <- function(mat.list, row.index, col.index, years.obs, years.est = F, ...){
  ts <- as.numeric(sapply(mat.list, FUN = "[", row.index, col.index))
  if(!years.est){
    years <- years.obs # if only for observed years
  }else{
    years <- c(years.obs, years.est)
  }
  plot(years, ts, xlab = "Years", type = "l", ...)
  #lines(years.est, ts[length(years.obs) + 1], col = "red")
  # TODO: auch für extrapolated values anwendbar machen: years.obs, years.est
}



#' scales matrix to 100.
#'
#' @param mat matrix
#' @param baseyear numeric, year that should be 100
#' @return a matrix
#' @export

scaleTo100 <- function(mat, baseyear){
  mat100 <- t(apply(as.matrix(mat), 1, function(x){100 * (x/as.matrix(mat)[which(rownames(mat) == as.character(baseyear)),])}))
  return(mat100)
}



#' Barplots to compare scenarios according to Region/INdusstry etc..
#'
#' @param dt.list list with data.tables. Each element of the list represents one scenario. The data.tables need to have 2 columns: one with Country/Industry etc, one with the values. If legend = T the names of the list elements need to the scenario names
#' @param colorvec vector with colorcodes 
#' @param yaxt.steps  steps for drawing tick marks at x-axis
#' @param sectors A character vector with names of the Regions/Industries etc. that want to be plotted. They need to be present in the first column of each data.table
#' @param legend boolean,  draw a legend??
#' @param legend.pos coordinates (c(x,y) or position (e.g."bottomleft") of the legend
#' @param ylim.adjust adjusts the upper limit of the y-axis. Adjust if you want more space for the legend
#' @return a plot
#' @export



scenarios_barplot <- function(dt.list, 
                              sectors,
                              scale = F,
                              colorvec = 1:100,
                              grid = F,
                              yaxt.steps,
                              ylim.adjust = 0, # adjust the upper limit of y-axis
                              legend = T, 
                              legend.pos = "topright", 
                              ...
){
  library(data.table)
  dt.list <- lapply(dt.list, setorder) # order data.tables alphabetically 
  for(i in 1:length(dt.list)){ # selecting countries/industries
    if(names(dt.list[[1]])[1] == "Country") dt.list[[i]] <- dt.list[[i]][Country %in% sectors]
    if(names(dt.list[[1]])[1] == "Industry") dt.list[[i]] <- dt.list[[i]][Industry %in% sectors]
  }
  if(scale){
    mat <- matrix(unlist(rbindlist(lapply(dt.list, "[", , 2))), nrow = length(sectors), ncol = length(dt.list))
    newmat <- mat
    for(i in 1:length(sectors)){
      if(max(mat[i,]) == 0) newmat[i,] <- mat[i,]
      else{
        newmat[i,which.max(mat[i,])] <- 100
        newmat[i,-which.max(mat[i,])] <- (100/mat[i,which.max(mat[i,])]) * mat[i,-which.max(mat[i,])]
      }
    }
    for(i in 1:length(dt.list)){
      dt.list[[i]][,2] <- newmat[,i]
    }
    ticks <- seq(0, 100, yaxt.steps)
  }
  
  x_values <- barplot(height = rep(0, length(sectors)), 
                      space = c(rep(length(dt.list), length(sectors)-1), length(dt.list) + 2), col = 0, ylim = c(0, max(unlist(lapply(dt.list, "[", ,2))) + ylim.adjust), 
                      las = 2, yaxt = "n", border = NA)
  if(!scale) ticks <- seq(0, signif(max(unlist(lapply(dt.list, "[", ,2))), 1), yaxt.steps) # set ticks for the y axis
  axis(2, at = ticks[c(T, F)], labels = ticks[c(T, F)], las = 2) # draw y axis
  if(grid) abline(h = ticks, col = "grey30", lty = 2, lwd = 0.5)
  
  for(i in 1:length(dt.list)){
    barplot(height = dt.list[[i]]$V1, las = 2, 
            space = c(length(dt.list)+i-1,rep(length(dt.list), length(sectors) - 1)), 
            col = colorvec[i], add = T, axes = F, ...)
    
    
  }
  x_values[length(x_values)] <- x_values[length(x_values)] - 2
  mtext(side = 1, text = unlist(dt.list[[1]][,1]), at = x_values+(length(dt.list)/2)-.5, las = 2, line = 1, cex = 0.7)
  if(legend) legend(legend.pos, legend = names(dt.list), fill = colorvec,box.col = "white", bg = "white", border = colorvec)
}

#' stacked area plot.
#'
#' @param data either matrix with Years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
#' @param agg aggregated? calls aggregate_matrix function. Boolean
#' @param agg.level aggregation level. Required if agg = T.
#' @param colorvec vector with colorcodes (length(colorvec) = ncol(mat))
#' @param xaxt.steps  steps for drawing tick marks at x-axis
#' @param yaxt.steps  steps for drawing tick marks at x-axis
#' @param legend boolean,  draw a legend??
#' @param legend.pos coordinates (c(x,y) or position (e.g."bottomleft") of the legend
#' @return a plot
#' @export


stacked_area_plot <-function(data, # either a matrix with Years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
                             agg = F,
                             agg.level,
                             colorvec = 1:100, # vector with colorcodes (length(colorvec) = ncol(mat))
                             xaxt.steps, # steps for drawing tick marks at x-axis
                             yaxt.steps, # steps for drawing tick marks at x-axis
                             ylim.adjust = 0, # adjust the upper limit of y-axis
                             legend = T, # draw a legend??
                             legend.pos = "topleft", # coordinates (c(x,y) or position (e.g."bottomleft") of the legend, see ?legend)
                             legend.ncol = 1,
                             legend.bty = "o",
                             ... # additional arguments for plot(), see ?plot
){
  if(is.data.table(data)){
    dt <- data
    names   <- names(dt)
    if(names[1] != "Year" & names[1] != "year") stop("First column of data table needs to contain the Years! Name of the column either <Year> or <year> ")
    Years   <- unique(dt[[names[1]]])
    sectors <- unique(dt[[names[2]]])
    mat <- matrix(dt[[names[3]]], ncol = length(sectors), nrow = length(Years), byrow = T)
    print(dim(mat))
    colnames(mat) <- sectors
    rownames(mat) <- Years
  }else{
    mat <- data
    Years <- rownames(mat)
  } 
  if(agg){
    if(is.null(agg.level)) stop("agg.level must be specified!")
    mat <- aggregate_matrix(mat, agg.level)
  }
  mat <- cumulate_matrix(mat)
  xx <- c(Years, rev(Years)) # create x-values of the polygons (same for all)
  yy_mat <- matrix(0, nrow = ncol(mat), ncol = 2 * nrow(mat)) # matrix to store y-values of each polygon
  for(i in 1:ncol(mat)){
    # fill y-values
    yy_mat[i,] <- c(rep(0, nrow(mat)), rev(mat[,ncol(mat) + 1 - i]))
  }
  #if(plot.ylim == NULL) plot.ylim <- 1.1*c(0, max(mat[,ncol(mat)]))
  plot(x=Years, y=mat[,ncol(mat)], col=colorvec[1], type='l', ylim=1.1*c(0, max(mat[,ncol(mat)])+ ylim.adjust) ,
       bty= "n", xaxt = "n", yaxt = "n", ...)
  axis(1, at = seq(min(Years), max(Years), xaxt.steps), lwd.ticks = 0.5, lwd = 0.5)
  axis(2, at = seq(0, signif(max(mat[,ncol(mat)]), 2), yaxt.steps), las = 2, lwd.ticks = 0.5, lwd = 0.5)
  abline(v = seq(min(Years), max(Years), xaxt.steps), col = "grey30", lty = 2, lwd = 0.5)
  abline(h = seq(0, signif(max(mat[,ncol(mat)]), 2), yaxt.steps), col = "grey30", lty = 2, lwd = 0.5)
  for(i in 1:ncol(mat)){
    polygon(xx, yy_mat[i,], col=colorvec[i], border=colorvec[i])
  }
  # drawing a legend if required
  if(legend == T){
    if(is.character(legend.pos)) legend(legend.pos, legend = rev(colnames(mat)),
                                        fill = colorvec,
                                        bty = legend.bty, 
                                        box.col = "white", bg = "white",
                                        border = colorvec,
                                        ncol = legend.ncol 
                                        #x.intersp = 0.5, 
                                        #text.width = 4
    )
    else legend(legend.pos[1], legend.pos[2], legend = rev(colnames(mat)),
                fill = colorvec,
                bty = "n", #box.col = "white",bg = "white"
                border = colorvec
    )
  }
}

#' colwise cumulate values of a matrix (required for stacked_area_plot)
#'
#' @param mat matrix
#' @param order boolean
#' @return a cumulated matrix
#' @export

cumulate_matrix <- function(mat, order = T){
  # colwise cumulate values of a matrix (required for stacked_area_plot)
  if(order == T){
    if(length(which(colnames(mat) == "Other")) == 1){ # sector "Other" as first col --> better for plotting
      new_mat <- mat[,-which(colnames(mat) == "Other")]
      new_mat <- new_mat[,order(colMeans(new_mat))]
      new_mat <- cbind("Other" = mat[,which(colnames(mat) == "Other")], new_mat)
    }else new_mat <- mat[,order(colMeans(mat))]
  }else{new_mat <- mat}
  for(i in 2:ncol(new_mat)){
    new_mat[,i] <- new_mat[,i-1] + new_mat[,i]
  }
  return(new_mat)
}

##################################################################################################################### #

#' adds names of industrie and region plus rank to each row of the data.table that results from the validate_forecast function.
#'
#' @param mat.valid data.table. Output of the function validate_forecast
#' @param names data.table which contains names of the countries and industries (length = 7987)
#' @param Y.obs4ranks Y matrix for one year for which the flows shall be ranked
#' @return a data.table with the following columns: .id, Rank, Country, Industry, FD_category, Year_split, Year_forecast, est, obs
#' @export

add_names2vali_Y <- function(mat.valid, names, Y.obs4ranks){
  FD.names <- c("Households", "NPISH", "Government", "GFCF")
  names4vali <- cbind(.id = unique(mat.valid$.id), 
                      as.data.table(unlist(lapply(names$Country, rep, times = 4))),
                      as.data.table(unlist(lapply(names$Industry, rep, times = 4))), 
                      as.data.table(rep(FD.names, 7987)))
  setnames(names4vali, c(".id", "Country", "Industry", "FD_category"))
  # calculate ranks
  Y.ranks <- as.data.table(matrix(rank(-Y.obs4ranks), ncol = ncol(Y.obs4ranks), nrow = nrow(Y.obs4ranks)))
  Y.ranks <- as.data.table(unlist(transpose(Y.ranks)))
  Y.ranks <- cbind(.id = unique(mat.valid$.id), Y.ranks)
  
  # attach ranks to data.table
  Y.names4valid <- merge(Y.ranks, names4vali)
  setnames(Y.names4valid, "V1", "Rank")
  # full data.table
  return(merge(Y.names4valid, mat.valid))
  
} 
##################################################################################################################### #
#' calculate pearson R2 for different values of year2future, year.split and ranks
#'
#' @param dt
#' @param mat.valid data.table. Output of the function validate_forecast
#' @param names data.table which contains names of the countries and industries (length = 7987)
#' @param Y.obs4ranks Y matrix for one year for which the flows shall be ranked
#' @return a data.table with the following columns: .id, Rank, Country, Industry, FD_category, Year_split, Year_forecast, est, obs
#' @export

pearson <- function(dt, ranks, years.forecast, parallel = F, n.cores){
  #years.forecast  <- unique(dt$Year_forecast)
  years.split     <- unique(dt$Year_split) 
  pars <- expand.grid(ranks, years.forecast, years.split)
  
  if(parallel){
    cl <- makeCluster(n.cores)
    clusterExport(cl=cl, list("dt", "pars"))
    clusterCall(cl, function() library(data.table))
    temp <- parLapply(cl, 1:nrow(pars), function(x) cor(dt[Year_forecast - Year_split == pars[x,2] & Rank < pars[x,1] & Year_split == pars[x,3]]$est,  
                                                 dt[Year_forecast - Year_split == pars[x,2] & Rank < pars[x,1] & Year_split == pars[x,3]]$obs)^2)
    stopCluster(cl)
    temp <- unlist(temp)
  }else{
    temp <- apply(pars, 1, function(x) cor(dt[Year_forecast - Year_split == x[2] & Rank < x[1] & Year_split == x[3]]$est,  
                                           dt[Year_forecast - Year_split == x[2] & Rank < x[1] & Year_split == x[3]]$obs)^2)
  }
  
  cor.mat <- array(dim = c(length(ranks), length(years.forecast), length(years.split)))
  l = 1
  for(k in 1:length(years.split)){
    for(j in 1:length(years.forecast)){
      for(i in 1:length(ranks)){
        cor.mat[i,j,k] <- temp[l]
        l <- l + 1
      }
    }
  }
   #for(i in 1:length(ranks)){
  #  for(j in 1:length(years.forecast)){
  #    for(k in 1:length(years.split)){
  #      cor.mat[i,j, k] <- cor(dt[Year_forecast - Year_split == years.forecast[j] & Rank < ranks[i]& Year_split == years.split[k]]$est,  dt[Year_forecast - Year_split == years.forecast[j]  & Rank < ranks[i]& Year_split == years.split[k]]$obs)^2
  #    }
  #  }
  #}
  return(cor.mat)
}

###################################################################################################################### #

#' plot valid
#'
#' @param cor.array correlation array. Output from pearson-function. Or list of correlation arrays
#' @return 
#' 
#' @export

plot_cv <- function(cor.array, years.split){
  #[rank, forecast, split]
  #par.old <- par(no.readonly = T)
  if(is.array(cor.array)){
    par(mfrow = c(3,4), mar = c(1,1,1,1))
    for(k in 1:length(years.split)){
      plot(year2future, cor.array[1,,k], type = "b", ylim = c(0, max(cor.array, na.rm = T)), main = years.split[k], lwd = 1.5)
      for(i in 2:length(ranks)){
        lines(year2future, cor.array[i,,k], type = "b", pch = i, col = i, lwd = 1.5)
      }
    }
    plot(year2future, cor.array[1,,k], ylim = c(0, max(cor.array, na.rm = T)), ylab = "", xlab = "", type = "n", yaxt = "n", xaxt = "n", bty = "n")
    legend("topleft", legend = ranks, col = 1:length(ranks), pch = 1:length(ranks), bty = "n")    
  }else{
    if(!is.list(cor.array)) stop("argument should be either array (3 dimensions) or a list of arrays!")
    par(mfrow = c(3,4), mar = c(1,1,1,1))
    for(k in 1:length(years.split)){
      plot(year2future, cor.array[[1]][1,,k], type = "n", ylim = c(0, max(sapply(cor.array, max, na.rm = T))), main = years.split[k], lwd = 1.5)
      for(j in 1:length(cor.array)){
        for(i in 1:length(ranks)){
          lines(year2future, cor.array[[j]][i,,k], type = "b", pch = i, col = j, lwd = 1.5)
        }
      }
    }
    plot(year2future, cor.array[[1]][1,,k], ylab = "", xlab = "", type = "n", yaxt = "n", xaxt = "n", bty = "n")
    legend("topleft", legend = names(cor.array), col = 1:length(cor.array), bty = "n", lty = 1)
    legend("topright", legend = ranks, bty = "n",  pch = 1:length(ranks))
  }
  #par(par.old)
}


#
##################################################################################################################### #

#' function for reproducing error message: "Fehler in .subset(x, j) : ungültiger Indextyp 'list'"
#'
#' @export

test_fun <- function(ts) {
  return(ts[ , lapply(ts, scale)])
  #[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))])
  #return(as.data.table(apply(ts, 2, scale)))
}




