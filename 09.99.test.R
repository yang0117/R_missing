#09.99.test

rm(list=ls())
source("09.01.beta_matrix_generator.R")
source("09.02.result_table_generate.R")
source("09.03.run_table.R")

############################
#test for beta_matrix_generator
res1 <- beta_list_all_method(n_sim=10,n=100,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2)
res1_no_intercept <- beta_list_all_method(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=0)
res2 <- beta_list_all_method(n_sim=50,n=50,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2)

res_all_beta <- beta_list_all_method(n_sim=10,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2)

res17 <- beta_list_all_method17(n_sim=15,n=50,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=0,
                                method_indicator="xy",loss_rate=0.62,logistic_method="xy",n_iter_SCAD=2,n_iter_MCP=2,
                                error_var=1,y_logistic=F,
                                initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                x_missing_location=1,error_independent=F,
                                lambda_location_SCAD="all",lambda_location_MCP="all")



############################
#test for L_infinity_norm
test_beta_est1 <- matrix(1:12,3,4)
#test for beta_length(should report an error)
test_beta_true <- 5:6
L_inf(test_beta_est1, test_beta_true)
#test 
test_beta_true <- 5:8
L_inf(test_beta_est1, test_beta_true)

############################
#test for relative_ratio
test_beta_est1 <- matrix(1:12,3,4)
#test for beta_length(should report an error)
test_beta_true <- 5:6
relative_bias(test_beta_est1, test_beta_true)
#test 
test_beta_true <- 5:8
relative_bias(test_beta_est1, test_beta_true)

############################
#test for table_all_t0n0
#use hand input dataset
test_n <- 10000
#test: (0,.5,1,0,0,1)
#true:(1, 0,1,0,0,1)
test_beta_est <- cbind(0,rbinom(test_n,1,0.5),rnorm(test_n,5),0,0,rnorm(test_n,0))
test_beta_vec <- c(1,0,1,0,0,1)
table_all_t0n0(test_beta_est,test_beta_vec)


############################
#test for table_COU
#use hand input dataset
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
test_n <- 10
test_beta_est <- cbind(0,rbinom(test_n,1,0.5),rnorm(test_n,5),0,rbinom(test_n,1,0.5),rnorm(test_n,0))
test_beta_est
test_beta_vec <- c(0,1,1,0,0,1)
table_COU(test_beta_est,test_beta_vec) == c(0.3,0.5,0.2)



#use true dataset from sample generator
beta_full <- res1$beta_method1_full[,-1]
#dim(beta_full)
#head(beta_full)
table_all_t0n0(beta_full,c(1.5,2,3,rep(0,5)))
beta_complete <- res1$beta_method2_complete[,-1]
#dim(beta_complete)
#head(beta_complete)
table_all_t0n0(beta_complete,c(1.5,2,3,rep(0,5)))


beta_full <- res1_no_intercept$beta_method1_full[,-1]
#dim(beta_full)
table_all_t0n0(beta_full,c(1.5,2,3,rep(0,5)))
beta_complete <- res1_no_intercept$beta_method2_complete[,-1]
#dim(beta_complete)
table_all_t0n0(beta_complete,c(1.5,2,3,rep(0,5)))

############################
#test for beta_est_filter
#use hand input dataset
test_n <- 10
#est: (0,.5,1,0,0,1)
test_beta_est <- cbind(0,rbinom(test_n,1,0.5),rnorm(test_n,5),0,0,rnorm(test_n,0))
test_beta_est
test_beta_vec <- c(1,0,1,0,0,0)

beta_est_filter(beta_matrix=test_beta_est,beta_vec=test_beta_vec,1.1)

############################
#test for beta_est_filter_bias
#use hand input dataset
test_n <- 10
#est: (0,.5,1,0,0,1)
test_beta_est <- cbind(0,rbinom(test_n,1,0.5),rnorm(test_n,5),0,0,rnorm(test_n,0))
test_beta_est
test_beta_vec <- c(1,0,5,0,0,0)

beta_est_filter_bias(beta_matrix=test_beta_est,beta_vec=test_beta_vec,1.5)


############################
#test for result_table_single
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
test_n <- 10
test_beta_est <- cbind(0,rbinom(test_n,1,0.5),rnorm(test_n,5),0,rbinom(test_n,1,0.5),rnorm(test_n,0))
test_beta_est
test_beta_vec <- c(0,1,1,0,0,1)

result_table_single(beta_result_matrix=test_beta_est,beta_vec=test_beta_vec,method_name="test!!!!!",filter_vec=c(1,0,10))

############################
#test for result_table_all
rm(list=ls())
source("09.03.run_table.R")
res17 <- beta_list_all_method17(n_sim=15,n=50,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=0,
                                method_indicator="xy",loss_rate=0.62,logistic_method="xy",n_iter_SCAD=2,n_iter_MCP=2,
                                error_var=1,y_logistic=F,
                                initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                x_missing_location=1,error_independent=F,
                                lambda_location_SCAD="all",lambda_location_MCP="all")
#res_intercept <- result_table_all(beta_result_list=res17,beta_vec=c(1.5,2,3,rep(0,5)),filter_vec=c(0.5,1,2))
#res_without_intercept <- result_table_all(beta_result_list=res1_no_intercept,beta_vec=c(1.5,2,3,rep(0,5)),filter_vec=c(0.5,1,2))
all_beta_table <- result_table_all(beta_result_list=res17,beta_vec=c(1.5,2,3,rep(0,5)),filter_vec=c(0.5,1,2),
                                   n_iter_SCAD = 2, n_iter_MCP = 2)

save.image(file = "09.full_complete.Rdata")

  
############################
#test for 09.03. an overall function to generate the result table  
table_res1 <- run_table(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,
                        filter_vec=c(0.5,1,2))
table_res1
save.image("table_all.Rdata")

############################
#test for 09.03 with three new parameters method_indicator,loss_rate,penalty
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table(n_sim=2,n=50,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,
                        filter_vec=c(0.5,1,2),method_indicator = "exp", loss_rate = 0.8, penalty = "lasso")
table_res1


############################
#test for 09.03 for run_table17
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=10,n=80,beta_vector=c(3,1.5,0.5,rep(0,5)),k=5,intercept=0,
                          filter_vec=c(0.05,0.5,1,2),method_indicator="xy",loss_rate=0.65,
                          logistic_method="xy",
                          n_iter_SCAD=2,n_iter_MCP=2,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD = F,
                          initial_true_indicator_MCP = F,
                          x_missing_location=1,error_independent=F,
                          lambda_location_SCAD="all",lambda_location_MCP="all")
                          
table_res1$result_list[[1]]
table_res1$result_list[[2]]
table_res1$result_list
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "method17A.Rdata")

###############Run all scenario
#Scenario A
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=10,n=100,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=0,
                          filter_vec=c(0.5,1),method_indicator = "exp", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_A.Rdata")

###############Run all scenario
#Scenario B
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(.5,1,2,rep(0,97)),k=5,intercept=0,
                          filter_vec=c(0.5,1,2),method_indicator = "positive", loss_rate = 0.8,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_B.Rdata")

###############Run all scenario
#Scenario C
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(.5,1,2,rep(0,5)),k=5,intercept=0,
                          filter_vec=c(0.5,1,2),method_indicator = "positive", loss_rate = 0.8,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=0.9,y_logistic=F,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_C.Rdata")

###############Run all scenario
#Scenario D
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(.5,1,2,rep(0,97)),k=5,intercept=0,
                          filter_vec=c(0.5,1,2),method_indicator = "positive", loss_rate = 0.8,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=0.9,y_logistic=F,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_D.Rdata")

###############Run all scenario
#Scenario E
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=200,beta_vector=c(.5,1,2,rep(0,5)),k=5,intercept=0,
                          filter_vec=c(0.5,1,2),method_indicator = "positive", loss_rate = 0.8,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=T,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_E.Rdata")

###############Run all scenario
#Scenario F
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=200,beta_vector=c(.5,1,2,rep(0,197)),k=5,intercept=0,
                          filter_vec=c(0.5,1,2),method_indicator = "positive", loss_rate = 0.8,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=T,
                          initial_true_indicator_SCAD=T,initial_true_indicator_MCP=T)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Method17_Scenario/Scenario_F.Rdata")




######################################
###############test for Scenario
######################################

############################
#beta2_missing_positive
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "positive", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_missing_positive.Rdata")
load("./Scenario_Test/beta2_missing_positive.Rdata")

############################
#beta2_missing_xy
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_missing_xy.Rdata")

############################
#beta2_missing_exp
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "exp", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_missing_exp.Rdata")

#########################################################

############################
#beta3_missing_positive
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "positive", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_missing_positive.Rdata")

############################
#beta3_missing_xy
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=60,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_missing_xy.Rdata")

############################
#beta3_missing_exp
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "exp", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_missing_exp.Rdata")







############################
#test for missing variable position

############################
#beta2_xy_position_2
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_xy_position_2.Rdata")

############################
#beta2_xy_position_3
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=3)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_xy_position_3.Rdata")

############################
#beta2_xy_position_8
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=8)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta2_xy_position_8.Rdata")

############################
#beta3_xy_position_2
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_xy_position_2.Rdata")

############################
#beta3_xy_position_3
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=3)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_xy_position_3.Rdata")

############################
#beta3_xy_position_8
rm(list=ls())
set.seed(123)
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.5,1),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=5,n_iter_MCP=5,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=8)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test/beta3_xy_position_8.Rdata")




##############test for iteration method for SCAD and MCP(cross-validation to choose lambda)
#Scenario_beta_2_n_100
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_cv_SCAD_MCP_1207/Scenario_beta_2_n_100.Rdata")

#Scenario_beta_2_n_300
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=300,beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_cv_SCAD_MCP_1207/Scenario_beta_2_n_300.Rdata")

#Scenario_beta_3_n_100
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_cv_SCAD_MCP_1207/Scenario_beta_3_n_100.Rdata")

#Scenario_beta_3_n_300
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=300,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_cv_SCAD_MCP_1207/Scenario_beta_3_n_300.Rdata")

#Scenario_beta_3_n_500
rm(list=ls())
source("09.03.run_table.R")
table_res1 <- run_table17(n_sim=100,n=500,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=2)
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_cv_SCAD_MCP_1207/Scenario_beta_3_n_500.Rdata")



#########################################################
#test for Scenario_Test_beta_order

# c(3,1.5,rep(0,2),2,rep(0,3))
rm(list=ls())
source("09.03.run_table.R")
set.seed(123)
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=c(1,2))
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_beta_order/data/Scenario_beta_3_1.5.Rdata")

# c(1.5,2,3,rep(0,5))
rm(list=ls())
source("09.03.run_table.R")
set.seed(123)
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(1.5,2,0,0),k=5,intercept=5,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=c(1,2))
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_beta_order/data/Scenario_beta_1.5_2.Rdata")

# c(3,1.5,rep(0,2),2,rep(0,3)) loss_.9999
rm(list=ls())
source("09.03.run_table.R")
set.seed(123)
table_res1 <- run_table17(n_sim=50,n=100,beta_vector=c(1.5,2,0,0),k=5,intercept=0,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=c(2,3))
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_beta_order/data/Scenario_beta_3_1.5_.9999.Rdata")

# c(1.5,2,3,rep(0,5)) loss_.9999
rm(list=ls())
source("09.03.run_table.R")
set.seed(123)
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2)),k=5,intercept=5,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 1,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=c(2,3))
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_beta_order/data/Scenario_beta_1.5_2_0.9999.Rdata")

# c(2,0.5,rep(0,2)) loss_.9999
rm(list=ls())
source("09.03.run_table.R")
set.seed(123)
table_res1 <- run_table17(n_sim=100,n=100,beta_vector=c(2,0.5,rep(0,2)),k=5,intercept=5,
                          filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 1,
                          n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                          initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                          x_missing_location=c(2,3))
table_res1$result_list[[1]]
dim(table_res1$result_table)[1]/4
table_res1
save(table_res1,file = "./Scenario_Test_beta_order/data/Scenario_beta_1.5_2_0.9999.Rdata")



############################
#test for scenario
#test for 10.01
rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(100)

beta_list <- list(beta_2_0=c(2,0.5,rep(0,2),1.5,rep(0,3)),
                  beta_0.5_2=c(0.5,1.5,2,rep(0,5)),
                  beta_3_0=c(3,1.5,rep(0,2),2,rep(0,3)),
                  beta_1.5_3=c(1.5,2,3,rep(0,5)))
intercept_vec <- c(0)
error_independent_vec=c(F)
x_missing_location_vec=c(1,2)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(intercept_vec)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=intercept_vec[k],
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h])
          file_location=paste("./Scenario_test_1220/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_intercept_",intercept_vec[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}


############################
#test for scenario
#test for 12.23
rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(200)
beta_list <- list(beta_2_1.5=c(2,1.5,0.5,rep(0,5)),
                  beta_3_2=c(3,2,1.5,rep(0,5)))
lambda_location <- list(l1_60=1:60,
                    l1_30=1:30,
                    l20_50=20:50)
error_independent_vec=c(T,F)
x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste("./Scenario_Test_1223/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}

############################
#test for scenario
#test for 12.23 sample 500
rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(500)

beta_list <- list(beta_2_1.5=c(2,1.5,0.5,rep(0,5)),
                  beta_3_2=c(3,2,1.5,rep(0,5)))

lambda_location <- list(l1_60=1:60,
                        l1_30=1:30,
                        l20_50=20:50)

error_independent_vec=c(T,F)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste("./Scenario_Test_1223/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}

############################
#test for scenario
#test for 160105 

#' note:drgaile_project02 2,1.5,1
#'      drgaile_project03 3,2,1
#'      drgaile_project04 3,2,0.5
#'      drgaile_project05 3,1.5,0.5

rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(200)

beta_list <- list(beta_3_1.5_0.5=c(3,1.5,0.5,rep(0,5)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T,F)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste("./Scenario_160105/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}

############################
#test for logistics scenario
#test for 160113

#' note:drgaile_project02 2,1.5,1
#'      drgaile_project03 3,2,1
#'      drgaile_project04 3,2,0.5
#'      drgaile_project05 3,1.5,0.5

rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(400,600)

beta_list <- list(beta_3_1.5_0.5=c(3,1.5,0.5,rep(0,5)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T,F)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=T,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste("./Logistics_160113/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}


############################
#test for logistics scenario
#test for 160113

#' note:drgaile_project02 2,1.5,1
#'      drgaile_project03 3,2,1
#'      drgaile_project04 3,2,0.5
#'      drgaile_project05 3,1.5,0.5

rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(400,200)

beta_list <- list(beta_3_2_1=c(3,2,1,rep(0,5)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T,F)

logistic_method=c("Fan_2001")


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(logistic_method)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "y_single", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=T,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=1,error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]],
                                    logistic_method=logistic_method[l])
          file_location=paste("./Logistic_Fan_2011/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_logistic_method_",logistic_method[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}


############################
#test for logistics scenario
rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(400,200,600)

number_non_zero <- 10
beta_t <- runif(number_non_zero,min=1,max=2)
beta_s <- rbinom(number_non_zero,1,0.5)
beta_s[which(beta_s==0)] <- -1

beta_ts <- beta_t*beta_s

beta_list <- list(beta_1000=c(beta_ts,rep(0,1000-number_non_zero)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T,F)

logistic_method=c("regular")


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(logistic_method)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "y_single", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=T,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=1,error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]],
                                    logistic_method=logistic_method[l])
          file_location=paste("./Logistic_Fan_2014/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_logistic_method_",logistic_method[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}

############################
#test for scenario
#test for 160126

#' note:drgaile_project02 2,1.5,1
#'      drgaile_project03 3,2,1
#'      drgaile_project04 3,2,0.5
#'      drgaile_project05 3,1.5,0.5

rm(list=ls())
source("09.03.run_table.R")

sample_size<- c(200)

beta_list <- list(beta_3_1.5_0.5=c(3,1.5,0.5,rep(0,197)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T,F)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=3,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste("./Scenario_160126/data/",names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}


############################
#test for scenario
#test for 160126

#!!!do not forget to change data store location to current directory

#' note:drgaile_project02 2,1.5,0.5/T
#'      drgaile_project03 3,1.5,0.5/T
#'      drgaile_project04 3,2,0.5/T
#'      drgaile_project05 2,1.5,0.5/F
#'      drgaile_project06 3,1.5,0.5/F
#'      drgaile_project07 3,2,0.5/F


rm(list=ls())
source("09.03.run_table.R")

current_path <- "./Scenario_160126/data/"

sample_size<- c(200)

beta_list <- list(beta_3_2_0.5=c(3,1.5,0.5,rep(0,5)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(T)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste(current_path,names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}

#hign dimension
#' note:drgaile_project02 2,1.5,0.5/T
#'      drgaile_project03 3,1.5,0.5/T
#'      drgaile_project04 3,2,0.5/T
#'      drgaile_project05 2,1.5,0.5/F
#'      drgaile_project06 3,1.5,0.5/F
#'      drgaile_project07 3,2,0.5/F

rm(list=ls())
source("09.03.run_table.R")

current_path <- "./Scenario_160126/data/"

sample_size<- c(200)

beta_list <- list(beta_200_2_1.5_0.5=c(3,2,0.5,rep(0,197)))

lambda_location <- list(l1_30=1:30)

error_independent_vec=c(F)

x_missing_location_vec=c(1,3,8)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(lambda_location)){
      for(h in 1:length(error_independent_vec)){
        for(l in 1:length(x_missing_location_vec)){
          set.seed(123)
          table_res1 <- run_table17(n_sim=100,n=sample_size[i],beta_vector=beta_list[[j]],k=5,intercept=0,
                                    filter_vec=c(0.05,0.1,0.3,0.5),method_indicator = "xy", loss_rate = 0.625,
                                    n_iter_SCAD=3,n_iter_MCP=3,error_var=1,y_logistic=F,
                                    initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                    x_missing_location=x_missing_location_vec[l],error_independent=error_independent_vec[h],
                                    lambda_location_SCAD=lambda_location[[k]],lambda_location_MCP=lambda_location[[k]])
          file_location=paste(current_path,names(beta_list)[j],"_n_",sample_size[i],
                              "_lambda_location_",names(lambda_location)[k],"_error_independent_",error_independent_vec[h],
                              "_x_missing_location_",x_missing_location_vec[l],".Rdata",sep = "")
          save(table_res1,file = file_location)
        }
      }
    }
  }
}


