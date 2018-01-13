setwd("/home/simon/Dokumente/Master/master_thesis/data/EB3")
Cf_mat <- read.csv("Cf_1995.csv")
# 4. zeile ist GWP100
indices_GWP100 <- which(Cf_mat[4,] != 0)
