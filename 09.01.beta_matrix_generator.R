#'09.01. beta_matrix_generate
#'
#####################################
#'beta_list_all_method:define a function used to generate the beta estimation for all three method
#'input: n: sample size
#'       beta_vector: beta value
#'       intercept: intercept for origianl data
#'       k: number of folder
#'       n_sim: simulation time
#'       method_indicator: indicator the method to use to generate missing indicator
#'       loss_rate: percentage not missing in the sample
#'       penalty: penalty type
#'output: a list contains all the beta value from estimation for method1(full), method2(complete), method3(our method)
#'        and beta_vector, intercept, k, n_sim
#'        all estimation in same row number are from same data set
#'        all of three method include intercept estimation, for method1(full), method2(complete),
#'        the estimation should be close to input intercept, for method3(our method), the estimation should be exactly 0
#'        

source("01.sample_generator.R")
source("01.01.gamma_caculator.R")
source("07.cv_esimator.R")
source("08.beta_guassian_our_method_estimation.R")

beta_list_all_method <- function(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,method_indicator,loss_rate,penalty){
  #beta name vecotr
  beta_name <- c("intercept")
  for (i in 1:length(beta_vector)){
    beta_name <- c(beta_name,paste("x",i,sep = ""))
  }
  
  #result matrix
  beta_method1_full <- matrix(-999,n_sim,length(beta_vector)+1)
  colnames(beta_method1_full) <- beta_name
  beta_method2_complete <- matrix(-999,n_sim,length(beta_vector)+1)
  colnames(beta_method2_complete) <- beta_name
  beta_method3_our <- matrix(-999,n_sim,length(beta_vector)+1)
  colnames(beta_method3_our) <- beta_name
  
  for(i in 1:n_sim){
    #generate the sample
    test_time <- proc.time()
    sample1 <- sample_generator1(n=n,beta_vector=beta_vector,intercept = intercept,loss_rate=loss_rate,method_indicator=method_indicator)
    #method1_full
    sample_full <- as.matrix(sample1$dataset)[,-1]
    print(paste("dim for full data is", paste(dim(sample_full),collapse=" ")))
    beta_method1_full[i,] <- beta_est_gaussian(sample_gaussian=sample_full,k=k,penalty=penalty)
    #method2_complete
    sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
    print(paste("dim for complete data is", paste(dim(sample_complete),collapse=" ")))
    beta_method2_complete[i,] <- beta_est_gaussian(sample_gaussian=sample_complete,k=k,penalty=penalty)
    #method3_our_method
    sample_logistic <- as.matrix(sample1$logistic_sample)
    print(dim(sample_logistic))
    beta_method3_our[i,] <- beta_est_logistics(sample_logistic=sample_logistic,sample_complete=sample_complete,k=k)$beta_lasso
    
    test_time <- proc.time() - test_time
    print(test_time)
    print(paste("simulation",i,"is done"))
  }
  result <- list(beta_method1_full=beta_method1_full,
                 beta_method2_complete=beta_method2_complete,
                 beta_method3_our=beta_method3_our,
                 simulation_time=n_sim,
                 sample_size=n,
                 beta=beta_vector,
                 intercept=intercept,
                 nfold=k,
                 lose_rate=loss_rate,
                 missing_method=method_indicator,
                 penalty=penalty)
  return(result)
}

#'beta_list_all_method17: define a function used to generate the beta estimation for all 17 method in the paper.
#'                        everything is exactly same as function "beta_list_all_method", except including all 17
#'                        methods and add two parameters,n_iter_SCAD,n_iter_MCP to indicate iteration times for SCAD
#'                        and MCP and delete penalty parameter
#'input: n: sample size
#'       beta_vector: beta value
#'       intercept: intercept for origianl data
#'       k: number of folder
#'       n_sim: simulation time
#'       method_indicator: indicator the method to use to generate missing indicator
#'       loss_rate: percentage not missing in the sample
#'       n_iter_SCAD: iteration time for SCAD, if 0, no SCAD will be estimated
#'       n_iter_MCP: iteration time for MCP, if 0, no MCP will be estimated
#'       error_var: variance for error term
#'       y_logistic: F: generate y as normal distribution
#'                   T: generate y as logistic regression, it will ignore the value of method_indicator and will keep
#'                      all 1 of y and 25% 0 of y
#'      initial_true_indicator_SCAD: F: use the lasso estimatior as initial to calculate weights
#'                                   T: use the true beta value as initial to calculate weights
#'      initial_true_indicator_MCP: F: use the lasso estimatior as initial to calculate weights
#'                                  T: use the true beta value as initial to calculate weights
#'output: a list contains all the beta value from estimation for method1(full), method2(complete), method3(our method)
#'        and beta_vector, intercept, k, n_sim
#'        all estimation in same row number are from same data set
#'        all of three method include intercept estimation, for method1(full), method2(complete),
#'        the estimation should be close to input intercept, for method3(our method), the estimation should be exactly 0
#'        



beta_list_all_method17 <- function(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,
                                   method_indicator,loss_rate,n_iter_SCAD,n_iter_MCP,
                                   error_var=1,y_logistic=F,
                                   initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                                   x_missing_location=x_missing_location,error_independent=error_independent,
                                   lambda_location_SCAD="all",lambda_location_MCP="all"){
  
  print("!!! important: This version has been modified to test scenario, DO NOT use this version before you confirm everything.
        Many parameter does not mean what they are in the original version.
        +Add list to store result without penalty")
  #######################
  ######initial all result matrix
  #######################
  #generate result matrix name vector
  result_name <- c("FLASSO","FSCAD","FMCP","CLASSO","CSCAD","CMCP","PLASSO")
  
  #prepare result matrix
  #beta name vecotr
  beta_name <- c("(intercept)")
  for (i in 1:length(beta_vector)){
    beta_name <- c(beta_name,paste("x",i,sep = ""))
  }
  result_matrix <- matrix(-999,n_sim,length(beta_vector)+1)
  colnames(result_matrix) <- beta_name

  #assign matrix to each element in the result_name vecotr
  for (i in 1:length(result_name)) {
    assign(result_name[i], result_matrix)
  }
  
  #create a list to store all result without penalty from FULL, COMPLETE and our method(logistic)
  NOPENALTY <- list(FULL=result_matrix,COMPLETE=result_matrix,LOGISTIC=result_matrix)
  
  #put all iteration result from SCAD to a list
  PSCAD <- list()
  for(i in 1:n_iter_SCAD){
    PSCAD[[i]] <- result_matrix
  }
  
  #put all iteration result from MCP to a list
  PMCP <- list()
  for(i in 1:n_iter_MCP){
    PMCP[[i]] <- result_matrix
  }
  
#   for (i in 1:n_iter_SCAD){
#     temp_name <- paste("PSCAD",i,sep = "")
#     result_name <- c(result_name,temp_name)
#   }
#   for (i in 1:n_iter_MCP){
#     temp_name <- paste("PMCP",i,sep = "")
#     result_name <- c(result_name,temp_name)
#   }
  
  #get the gamma value which are used to generate sample
  n_run <- 1000
  gamma_vec <- numeric(n_run)
  for (i in 1:n_run){
    gamma_vec[i] <- gamma_cal(n=n, beta_vector=beta_vector, intercept=intercept,
                              gamma=-999, gamma_estimator_switch=T,
                              loss_rate=loss_rate,
                              method_indicator=method_indicator,
                              error_var=error_var,
                              y_logistic=y_logistic,error_independent=error_independent)$gamma
  }
  
  gamma_est <- mean(gamma_vec)
  
  #######################
  ######assign value to all 17 result matrix
  #######################
  
  lasso_for_SCAD_count <- 0
  lasso_for_MCP_count <- 0
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
    n_row_full[i] <- dim(sample_full)[1]
    print(paste("dim for full data is", paste(dim(sample_full),collapse=" ")))
    #print(sample_full)
    FLASSO[i,] <- beta_est_gaussian(sample_gaussian=sample_full,k=k,penalty="lasso")
    FSCAD[i,] <- beta_est_gaussian(sample_gaussian=sample_full,k=k,penalty="SCAD")
    FMCP[i,] <- beta_est_gaussian(sample_gaussian=sample_full,k=k,penalty="MCP")
    NOPENALTY[[1]][i,] <- lm(sample_full[,1]~sample_full[,-1])$coefficients
    #complete data
    sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
    print(paste("dim for complete data is", paste(dim(sample_complete),collapse=" ")))
    n_row_complete[i] <- dim(sample_complete)[1] 
    #print(sample_complete)
    CLASSO[i,] <- beta_est_gaussian(sample_gaussian=sample_complete,k=k,penalty="lasso")
    CSCAD[i,] <- beta_est_gaussian(sample_gaussian=sample_complete,k=k,penalty="SCAD")
    CMCP[i,] <- beta_est_gaussian(sample_gaussian=sample_complete,k=k,penalty="MCP")
    NOPENALTY[[2]][i,] <- lm(sample_complete[,1]~sample_complete[,-1])$coefficients
    #our_method(logistic data)
    sample_logistic <- as.matrix(sample1$logistic_sample)
    n_row_logistic[i] <- dim(sample_logistic)[1]
    NOPENALTY[[3]][i,] <- c(0,glm(sample_logistic[,1]~sample_logistic[,-1]-1,family="binomial")$coefficients)
    print(dim(sample_logistic))
    logistic_res <- beta_est_logistics(sample_logistic=sample_logistic,sample_complete=sample_complete,k=k,
                                       n_iter_SCAD=n_iter_SCAD,n_iter_MCP=n_iter_MCP,
                                       lasso_for_SCAD_count = lasso_for_SCAD_count, lasso_for_MCP_count = lasso_for_MCP_count,
                                       beta_vector = beta_vector,
                                       initial_true_indicator_SCAD = initial_true_indicator_SCAD,
                                       initial_true_indicator_MCP = initial_true_indicator_MCP,
                                       lambda_location_SCAD=lambda_location_SCAD,lambda_location_MCP=lambda_location_MCP)
    lasso_for_SCAD_count <- logistic_res$lasso_for_SCAD_count
    lasso_for_MCP_count <- logistic_res$lasso_for_MCP_count
    #lasso
    PLASSO[i,] <- logistic_res$beta_lasso
    #SCAD
    SCAD_res <- logistic_res$beta_SCAD
    for (j in 1:n_iter_SCAD){
      PSCAD[[j]][i,] <- SCAD_res[j,]
    }
    #MCP
    MCP_res <- logistic_res$beta_MCP
    for (j in 1:n_iter_MCP){
      PMCP[[j]][i,] <- MCP_res[j,]
    }

    test_time <- proc.time() - test_time
    print(test_time)
    print(paste("simulation",i,"is done"))
  }
  
  result <- list(FLASSO=FLASSO,
                 FSCAD=FSCAD,
                 FMCP=FMCP,
                 CLASSO=CLASSO,
                 CSCAD=CSCAD,
                 CMCP=CMCP,
                 PLASSO=PLASSO,
                 PSCAD=PSCAD,
                 PMCP=PMCP,
                 NOPENALTY=NOPENALTY,
                 simulation_time=n_sim,
                 sample_size=n,
                 beta=beta_vector,
                 intercept=intercept,
                 nfold=k,
                 lose_rate=loss_rate,
                 missing_method=method_indicator,
                 error_var=error_var,
                 y_logistic=y_logistic,
                 lasso_for_SCAD_count=lasso_for_SCAD_count,
                 lasso_for_MCP_count=lasso_for_MCP_count, 
                 initial_true_indicator_SCAD = initial_true_indicator_SCAD,
                 initial_true_indicator_MCP = initial_true_indicator_MCP,
                 x_missing_location=x_missing_location,
                 gamma_est=gamma_est,
                 sample_full=sample_full,
                 sample_complete=sample_complete,
                 mean_sample_full=mean(n_row_full),
                 mean_sample_complete=mean(n_row_complete),
                 mean_sample_logistic=mean(n_row_logistic),
                 error_independent=error_independent,
                 lambda_location_SCAD=lambda_location_SCAD,
                 lambda_location_MCP=lambda_location_MCP)
  return(result)
}
  
