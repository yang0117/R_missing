rm(list=ls())
source("01.sample_generator.R")
source("02.simulation.R")

#set up parameter
n_sim=100
n_col=5
#prepare the table
#lasso
lasso_result <- matrix(0,n_sim,n_col)
colnames(lasso_result) <- c("tn0en0","tn0e0","t0en0","t0e0","lasso_lambda_est")
#SCAD
SCAD_result <- matrix(0,n_sim,n_col)
colnames(SCAD_result) <- c("tn0en0","tn0e0","t0en0","t0e0","SCAD_lambda_est")
#MCP
MCP_result <- matrix(0,n_sim,n_col)
colnames(MCP_result) <- c("tn0en0","tn0e0","t0en0","t0e0","MCP_lambda_est")
#result list to store all result
result_list <- list(lasso_result=as.data.frame(lasso_result),lasso_mean=rep(0,n_col),
               SCAD_result=as.data.frame(SCAD_result),SCAD_mean=rep(0,n_col),
               MCP_result=as.data.frame(MCP_result),MCP_mean=rep(0,n_col))

#function to generate result list
result_function <- function(result_list,simulation_time=100,
                            p=8,n=100,
                            gamma=1,gamma_estimator_switch=T,
                            loss_rate=0.8,
                            beta_vector=c(1.5,2,3,rep(0,5)),
                            nonzero_num=3){
  for(i in 1:simulation_time){
    print(i)
    all_result <- sim1_warning_free(p=p,n=n,
                                    gamma=gamma,gamma_estimator_switch=gamma_estimator_switch,
                                    loss_rate=loss_rate,
                                    beta_vector=beta_vector,
                                    nonzero_num=nonzero_num)
    print(all_result)
    result_list$lasso_result[i,]<-all_result$lasso
    result_list$SCAD_result[i,]<-all_result$SCAD
    result_list$MCP_result[i,]<-all_result$MCP
  }
  result_list$lasso_mean <- colMeans(result_list$lasso_result)
  result_list$SCAD_mean <- colMeans(result_list$SCAD_result)
  result_list$MCP_mean <- colMeans(result_list$MCP_result)
  return(result_list)
}

####################################
#Try different parameters:

#case1
#p=8 beta=(1.5, 2, 3, 5 zero)
result_case1 <- result_list
result_case1 <- result_function(result_list=result_case1,simulation_time=n_sim,
                                p=8,n=100,
                                gamma=1,gamma_estimator_switch=T,
                                loss_rate=0.8,
                                beta_vector=c(1.5,2,3,rep(0,5)),
                                nonzero_num=3)

# for(i in 1:n_sim){
#   print(i)
#   all_result <- sim1_warning_free(p=8,n=100,
#                                   gamma=1,gamma_estimator_switch=T,
#                                   loss_rate=0.8,
#                                   beta_vector=c(1.5,2,3,rep(0,5)),
#                                   nonzero_num=3)
#   print(all_result)
#   lasso_result[i,]<-all_result$lasso
#   SCAD_result[i,]<-all_result$SCAD
#   MCP_result[i,]<-all_result$MCP
# }


#case2
#p=50 beta=(1.5, 2, 3, 47 zero)
result_case2 <- result_list
result_case2 <- result_function(result_list=result_case2,simulation_time=n_sim,
                                p=50,n=100,
                                gamma=1,gamma_estimator_switch=T,
                                loss_rate=0.8,
                                beta_vector=c(1.5,2,3,rep(0,47)),
                                nonzero_num=3)

#case3
#p=50 beta=(300, 400, 500, 47 zero)
result_case3 <- result_list
result_case3 <- result_function(result_list=result_case3,simulation_time=n_sim,
                                p=50,n=100,
                                gamma=1,gamma_estimator_switch=T,
                                loss_rate=0.8,
                                beta_vector=c(300,400,500,rep(0,47)),
                                nonzero_num=3)

save.image("parameter_test.RData")
#test case
source("01.sample_generator.R")
source("02.simulation.R")
result_test <- result_list
result_test <- result_function(result_list=result_test,simulation_time=n_sim,
                                p=50,n=100,
                                gamma=1,gamma_estimator_switch=T,
                                loss_rate=0.8,
                                beta_vector=c(300,400,500,rep(0,47)),
                                nonzero_num=3)


