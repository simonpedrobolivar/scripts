require(data.table)
require(Rcpp)
require(RcppArmadillo)


path2data <- "/media/simon/dateien/master_thesis_data/"
setwd(path2data)

###################################
##### load data
###################################



load("/home/simon/Dokumente/Master/master_thesis/scripts/R/data/Y_validation_glm1.RData") # Y
load("/home/simon/Dokumente/Master/master_thesis/scripts/R/data/FY_validation_glm1.RData") # FY
load("/home/simon/Dokumente/Master/master_thesis/scripts/R/data/S_valid_glm.RData") # S
Cf_mat <- as.matrix(fread(paste0("Cf_1995.csv"), select = c(27 , 28 , 29 , 71 , 72 , 73 , 74 , 75 , 76 , 77 , 78,  96,  97, 427, 428, 429, 430, 431, 433, 439, 441, 442)))

years_split <- unique(Y_valid_list$Year_split)
years_forecast <- unique(Y_valid_list$Year_forecast)
ranks <- c(7987) # c(50, 100, 200, 7987) # number of largest flows included in calculations
for(i in 1:length(years_split)){
  for(j in 1:length(years_forecast)){
    for(k in 1:length(ranks)){
      starttime <- Sys.time()
      cat("---------------------", "\n")
      cat("Run", i * j * k, "(out of ", length(years_split) * length(years_forecast)  * length(ranks), ")\n")
      cat("Year split: ", years_split[i], " / Year forecast: ", years_forecast[j], " / Rank: ", ranks[k], "\n")
      cat("1. Load data \n")
      A_mat <- as.matrix(fread(paste0("A_", years_forecast[j], ".csv")))
      #Y_mat <- as.matrix(fread("Y_2011.txt", skip = 2, drop = c(1,2)))[,36:39]
      #FY_mat <- as.matrix(fread("FY_2011.csv"))
      #S_mat <- as.matrix(fread(paste0("S_", j, ".csv")))
      
      Y_mat   <- vec2mat(Y_valid_list[Year_split == years_split[i] & Year_forecast == years_forecast[j]]$est, n.row = 7987, n.col = 4)
      FY_mat  <- vec2mat(FY_valid_glm1[Year_split == years_split[i] & Year_forecast == years_forecast[j]]$est, n.row = 1330, n.col = 4)[c(27 , 28 , 29 , 71 , 72 , 73 , 74 , 75 , 76 , 77 , 78,  96,  97, 427, 428, 429, 430, 431, 433, 439, 441, 442),]
      S_mat   <- vec2mat(temp[Year_split == years_split[i] & Year_forecast == years_forecast[j]]$est, n.row = 22, n.col = 7987)
      cat("2. Calculate L-inverse and CO2-footprint \n")
      L_mat <- calc_L(A_mat, diag(1, 7987)) # calculate L inverse
      x <- L_mat %*% as.matrix(Y_mat)
      x <- apply(x, 1, sum)
      B <- as.matrix(S_mat) %*% diag(x, 7987)
      GWP <- Cf_mat %*% B
      cat("3. Save results \n")
      results <- list("GWP" = as.data.table(GWP), "FY" = FY_mat, "pars" = list("year_split" = years_split[i], "year_forecast" = years_forecast[j], "rank" = ranks[k], "model" = "glm1"))
      save(results, file = paste0("/home/simon/Dokumente/Master/master_thesis/Results/MRIO_Validation_Results/GWP_split", i, "_forecast", j, "_rank", k, "RData"))
      cat("Time needed:", Sys.time() - starttime, "\n")
      rm(GWP, results)
      }
    }
}

results$pars
sum(results$GWP[1,])/1E9

##################################
#### create matrices
##################################



##################################
##### calculate footprint
##################################
system.time(
L_mat <- solve(diag(1,7987) - A_mat)
) # 619 sec

  





library(rPython)  

python.exec("import numpy as np")
python.exec("import scipy")

system.time(
python.exec(c('from scipy import io', 'import numpy as np',
              'def calc_L2(path):',
              '\tA = scipy.io.loadmat(path)["EB3_A_ITC"]',
              '\treturn np.linalg.inv(np.eye(A.shape[0]) - A)' ))
) # 550 sec

system.time(python.call( "calc_L2", "/media/simon/dateien/master_thesis_data/2011_ITC.mat"))
















  