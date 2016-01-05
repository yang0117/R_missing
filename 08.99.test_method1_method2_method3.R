#08.test for method1 and method2
rm(list=ls())
source("01.sample_generator.R")
source("08.beta_guassian_our_method_estimation.R")

#generate sample
sample1 <- sample_generator1(n=100,beta_vector=c(3, 1.5, 2, rep(0,5)),intercept=0,
                             loss_rate = 0.625,method_indicator="xy")

##test for method_1(full_data)
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)
beta_est_gaussian(sample_gaussian=sample_full,k=5,penalty = "lasso")

sample_gaussian = sample_full
k=5
fit_cv_lasso <- cv.glmnet(sample_gaussian[,-1],sample_gaussian[,1],family="gaussian",nfolds=k,standardize = T)
fit_cv_lambda <- fit_cv_lasso$lambda.min
coef1 <- coef(fit_cv_lasso,s = fit_cv_lambda)
coef1

##test for method_2(complete_data(after deleting missing))
sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
dim(sample_complete)
beta_est_gaussian(sample_gaussian=sample_complete,k=5,penalty = "lasso")

##test for method_3(logsitcs_data(our method))
sample_logistic <- as.matrix(sample1$logistic_sample)
dim(sample_logistic)
beta_res1 <- beta_est_logistics(sample_logistic=sample_logistic,sample_complete=sample_complete,k=5)
beta_res1$beta_lasso


#test for SCAD MCP
rm(list=ls())
source("01.sample_generator.R")
source("08.beta_guassian_our_method_estimation.R")
res_list <- list()
for(i in 1:2){
  sample1 <- sample_generator1(n=100,beta_vector=c(3,1.5,rep(0,2),2,rep(0,3)),intercept = 0,method_indicator = "xy",loss_rate = 0.625,gamma_estimator_switch = T,error_independent = T)
  sample_full <- as.matrix(sample1$dataset)[,-1]
  sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
  sample_logistic <- as.matrix(sample1$logistic_sample)
  
  beta_res1 <- beta_est_logistics(sample_logistic=sample_logistic,sample_complete=sample_complete,k=5,n_iter_SCAD=3,n_iter_MCP=3)
  res_list[[i]]<-beta_res1
}




SCAD_iteration_weight <- function(lasso_est,lambda){
  res <- numeric(length(lasso_est))
  for (i in 1:length(res)){
    if(lasso_est[i] <= lambda) res[i] <- lambda
    else{
      num1 <- 3.7*lambda - lasso_est[i]
      num1 <- postive_fun(num1)
      res[i] <- num1/(3.7-1)
    }
  }
  return(res)
}
SCAD_iteration_weight(1:5,3)

#test for lambda_path_generator
lambda_test <- lambda_path_generator(3,0.01,30)
lam_length <- length(lambda_test)
diff_vec <- numeric(lam_length)
for(i in 1:(lam_length-1)){
  diff_vec[i] <- lambda_test[i] - lambda_test[i+1]
}
diff_vec
lambda_test_log <- log(lambda_test)
diff_vec_log <- numeric(lam_length)
for(i in 1:(lam_length-1)){
  diff_vec_log[i] <- lambda_test_log[i] - lambda_test_log[i+1]
}
diff_vec_log

#test for beta_est_weight

rm(list=ls())
source("01.sample_generator.R")
source("08.beta_guassian_our_method_estimation.R")
set.seed(123)
sample_test <- sample_generator1(n=50,beta_vector=c(2, 0.5, 1, rep(0,5)),intercept=0,
                                 loss_rate = 0.625,method_indicator="xy")
sample_logistic <- sample_test$logistic_sample
weights=seq(1,8,by=1)
weights=rep(1,8)
beta_test <- beta_est_weight(sample_logistic,weights)
beta_test

#test for beta_iteration_est
rm(list=ls())
source("01.sample_generator.R")
source("08.beta_guassian_our_method_estimation.R")
set.seed(123)
n_sim <- 5

for(i in 1:n_sim){
  if(i==1){
    sample_test <- sample_generator1(n=100,beta_vector=c(3, 1.5, 2, rep(0,5)),intercept=0,
                                     loss_rate = 0.625,method_indicator="xy")
    sample_complete <- as.matrix(subset(sample_test$dataset,missing_indicator==1,select = -missing_indicator))
    sample_logistic <- as.matrix(sample_test$logistic_sample)
    
    beta_iteration_res <- beta_iteration_est(sample_logistic=sample_logistic,sample_complete=sample_complete,
                                             k=5,n_iteration=3,weight_fun=SCAD_iteration_weight,lambda_location=40:55)
    beta_matrix <- beta_iteration_res$beta
    weight_matrix <- beta_iteration_res$weight
    lambda_vec <- beta_iteration_res$lambda
  }else{
    sample_test <- sample_generator1(n=100,beta_vector=c(2, 0.5, 1, rep(0,5)),intercept=0,
                                     loss_rate = 0.625,method_indicator="xy")
    sample_complete <- as.matrix(subset(sample_test$dataset,missing_indicator==1,select = -missing_indicator))
    sample_logistic <- as.matrix(sample_test$logistic_sample)
    
    beta_iteration_res <- beta_iteration_est(sample_logistic=sample_logistic,sample_complete=sample_complete,
                                             k=5,n_iteration=3,weight_fun=SCAD_iteration_weight,lambda_location=40:55)
    beta_matrix <- rbind(beta_matrix,beta_iteration_res$beta)
    weight_matrix <- rbind(weight_matrix,beta_iteration_res$weight)
    lambda_vec <- c(lambda_vec,beta_iteration_res$lambda)
  }
}

beta_matrix
weight_matrix
lambda_vec

