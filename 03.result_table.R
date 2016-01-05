#generate table
rm(list=ls())
source("01.sample_generator.R")
source("02.simulation.R")

#simulation time
n_sim=100
n_col=5
#make lasso SCAD MCP table
#lasso
lasso_result <- matrix(0,n_sim,n_col)
colnames(lasso_result) <- c("tn0en0","tn0e0","t0en0","t0e0","lasso_lambda_est")
lasso_result <- as.data.frame(lasso_result)
#SCAD
SCAD_result <- matrix(0,n_sim,n_col)
colnames(SCAD_result) <- c("tn0en0","tn0e0","t0en0","t0e0","SCAD_lambda_est")
SCAD_result <- as.data.frame(SCAD_result)
#MCP
MCP_result <- matrix(0,n_sim,n_col)
colnames(MCP_result) <- c("tn0en0","tn0e0","t0en0","t0e0","MCP_lambda_est")
MCP_result <- as.data.frame(MCP_result)

for(i in 1:n_sim){
  print(i)
  all_result <- sim1_warning_free()
  print(all_result)
  lasso_result[i,]<-all_result$lasso
  SCAD_result[i,]<-all_result$SCAD
  MCP_result[i,]<-all_result$MCP
}

