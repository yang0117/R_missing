#'10.01. oracle_beta_list
#'
#####################################
#'oracle_beta_list:define a function used to generate the oracle beta estimation for all three method(full,complete, proposed)
#'input: n: sample size
#'       beta_vector: beta value
#'       intercept: intercept for origianl data
#'       n_sim: simulation time
#'       method_indicator: indicat the method to use to generate missing indicator
#'       loss_rate: percentage not missing in the sample
#'       x_missing_location: for "xy" method, which missing location will be used, 1=x1,2=x2,...
#'       error_independent: F: generate the error matrix in 0.5^abs(i-j)
#'                          T: generate the independent error matrix
#'output: a list contains all the beta value from estimation for all three data type(full, complete, proposed)
#'        and beta_vector, intercept, k, n_sim, method_indicator, loss_rate,
#'        all estimation in same row number are from same data set
#'        all of three method include intercept estimation, for method1(full), method2(complete),
#'        the estimation should be close to input intercept, for method3(our method), the estimation should be exactly 0


source("01.sample_generator.R")
source("01.01.gamma_caculator.R")

oracle_beta_list <- function(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),intercept=2,
                                   method_indicator,loss_rate, error_var=1,y_logistic=F,
                                   x_missing_location=x_missing_location,error_independent){
  
  print("!!! important: This version has been modified to test scenario, DO NOT use this version before you confirm everything.
        Many parameter does not mean what they are in the original version.
        +Add list to store result without penalty")
  #######################
  ######initial all result matrix
  #######################
  
  #get the non-zero beta index
  beta_non_zero_ind <- which(beta_vector != 0)
  beta_with_intercept_ind <- c(1,beta_non_zero_ind+1)
  
  #generate result matrix name vector
  result_name <- c("FULL","COMPLETE","PROPOSED")
  
  #prepare result matrix
  #beta name vector
  beta_name <- c("(intercept)")
  for (i in 1:length(beta_vector)){
    beta_name <- c(beta_name,paste("x",i,sep = ""))
  }
  result_matrix <- matrix(-999,n_sim,length(beta_vector)+1)
  colnames(result_matrix) <- beta_name
  result_matrix <- result_matrix[,beta_with_intercept_ind]
  
  #assign matrix to each element in the result_name vecotr
  for (i in 1:length(result_name)) {
    assign(result_name[i], result_matrix)
  }
  
  #get the gamma value which are used to generate sample
  n_run <- 1000
  gamma_vec <- numeric(n_run)
  for (i in 1:n_run){
        gamma_vec[i] <- gamma_cal(n=n, beta_vector=beta_vector, intercept=intercept,
                                  gamma=-999, gamma_estimator_switch=T,
                                  loss_rate=loss_rate,
                                  method_indicator=method_indicator,
                                  error_var=error_var,
                                  y_logistic=y_logistic,
                                  error_independent=error_independent)$gamma
  }
  
  gamma_est <- mean(gamma_vec)
  
  n_row_full <- rep(-99,n_sim)
  n_row_complete <- rep(-99,n_sim)
  n_row_logistic <- rep(-99,n_sim)
  for(i in 1:n_sim){
    #generate the sample
    test_time <- proc.time()
    sample1 <- sample_generator1(n=n,beta_vector=beta_vector,intercept = intercept,loss_rate=loss_rate,
                                 method_indicator=method_indicator, error_var=error_var,y_logistic=y_logistic,
                                 gamma=gamma_est, gamma_estimator_switch=F,x_missing_location=x_missing_location,
                                 error_independent=error_independent)
    #full data
    sample_full <- as.matrix(sample1$dataset)[,-1]
    sample_full <- sample_full[,beta_with_intercept_ind]
    n_row_full[i] <- dim(sample_full)[1]
    print(paste("dim for full data is", paste(dim(sample_full),collapse=" ")))
    FULL[i,] <- lm(sample_full[,1]~sample_full[,-1])$coefficients
    print("mean(sample_full[,1]>-0.9*sd(sample_full[,1])+intercept)")
    print(mean(sample_full[,1]>-0.9*sd(sample_full[,1])+intercept))
    
    #complete data
    sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
    sample_complete <- sample_complete[,beta_with_intercept_ind]
    n_row_complete[i] <- dim(sample_complete)[1]
    print(paste("dim for complete data is", paste(dim(sample_complete),collapse=" ")))
    COMPLETE[i,] <- lm(sample_complete[,1]~sample_complete[,-1])$coefficients
    #our_method(logistic data)
    sample_logistic <- as.matrix(sample1$logistic_sample)
    sample_logistic <- sample_logistic[,beta_with_intercept_ind]
    n_row_logistic[i] <- dim(sample_logistic)[1]
    print(dim(sample_logistic))
    PROPOSED[i,] <- c(0,glm(sample_logistic[,1]~sample_logistic[,-1]-1,family="binomial")$coefficients)
    test_time <- proc.time() - test_time
    print(test_time)
    print(paste("simulation",i,"is done"))
  }
  
  oracle_beta_est <- list(FULL=FULL,
                          COMPLETE=COMPLETE,
                          PROPOSED=PROPOSED)
  
  
  result <- list(oracle_beta_est=oracle_beta_est,
                 simulation_time=n_sim,
                 sample_size=n,
                 beta=beta_vector,
                 intercept=intercept,
                 lose_rate=loss_rate,
                 missing_method=method_indicator,
                 error_var=error_var,
                 gamma_est=gamma_est,
                 y_logistic=y_logistic,
                 x_missing_location=x_missing_location,
                 beta_non_zero_ind=beta_non_zero_ind,
                 mean_sample_full=mean(n_row_full),
                 mean_sample_complete=mean(n_row_complete),
                 mean_sample_logistic=mean(n_row_logistic),
                 error_independent=error_independent)
  return(result)
}

