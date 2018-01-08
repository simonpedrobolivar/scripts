setwd("~/Dokumente/Master/master_thesis/data/EB3/Scenario_2")
A_mat <- fread("A_2015.csv", sep = ",")

New_Energy_Mix = c(0,0.07, 0, 0.04, 0.49, 0, 0.1, 0.07, 0.02, 0.01, 0.2, 0)
energy_input_per_industry <- apply(A_mat[911:922,], 2, sum)
A_new_mat <- A_mat
A_new_mat[911:922,] <- as.matrix(New_Energy_Mix) %*% t(as.matrix(energy_input_per_industry)) 


for m in range(0, DES_NoofCountries * DES_NoofIndustries): 
  sum_energy_inputs = MRIO_A[910:921,m].sum()
print(sum_energy_inputs)
for n in range(0, 11):
  MRIO_A[910 + n, m] = sum_energy_inputs * New_Energy_Mix[n]
MRIO_L = np.linalg.inv(np.eye(MRIO_A.shape[0]) - MRIO_A) # build IO model (leontief matrix)