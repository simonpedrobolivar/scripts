setwd("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables")

#I_mat <- diag(1,35 * 41) # A_mat is 1435 x 1435 (35 industries in 41 countries/regions)

for(i in 1995:2011){
  Z_mat_raw <- as.matrix(read.csv(paste("wiot_", i, ".csv" , sep = ""), skip = 6, header = F)[,-(1:4)]) # rows 1:6 and cols 1:4 are names, indices etc.
  x_vec <- Z_mat_raw[1435+8,1:1435] # total output of industry i in country j, == sum of inputs + total added value (consisting of taxes less subsidies on products, Cif/ fob adjustments on exports, Direct purchases abroad by residents, Purchases on the domestic territory by non-residents, Value added at basic prices)
  x_vec[which(x_vec == 0)]  <- 0.0001 # to avoid division by 0 (some industries in some countries have zero output)
  x_hat_mat <- diag(1/x_vec)
  A_mat <- Z_mat_raw[1:(35*41), 1:(35*41)] %*% x_hat_mat
  #L_mat <- solve(I_mat - A_mat) # L = (I-A)^-1
  write.table(x_vec, file = paste("x_vec", i, ".csv", sep = ""), row.names = F, col.names = F)
  write.table(A_mat, file = paste("A_matrix", i, ".csv", sep = ""), row.names = F, col.names = F)
  print(paste("A_mat", i, ".csv", " done", sep = ""))
}