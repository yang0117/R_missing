#08.01.function to get the beta estimation for gaussian dataset by lasso and cv.glmnet()
#      and function to get beta estimation from our method

#check package
if(!require("glmnet")){
  install.packages("glmnet")
  library(glmnet)
}
source("07.cv_esimator.R")

#beta_est_lasso_gaussian: function to get the beta estimation for gaussian dataset by lasso and cv.glmnet()
#                         Can be used for both full dataset and complete dataset(after deleting missing from full dataset)
#input: sample_gaussian(matrix): WITH intercept, full dataset or complete dataset(after deleting missing from full dataset)
#       k: number of folders for cross validation
#       penalty: lasso, SCAD,MCP 
#output: a vevtor of estimation of beta based on the lasso and cross validation method

beta_est_gaussian <- function(sample_gaussian=sample_full,k=k,penalty){
  if(penalty == "lasso"){
    fit_cv_lasso <- cv.glmnet(sample_gaussian[,-1],sample_gaussian[,1],family="gaussian",nfolds=k,standardize = T,intercept=T)
    fit_cv_lambda <- fit_cv_lasso$lambda.min
    coef1 <- coef(fit_cv_lasso,s = fit_cv_lambda)
    beta_est <- rep(0,coef1@Dim[1])
    x_ind <- 0
    for (i in coef1@i){
      x_ind <- x_ind +1
      beta_est[i+1] <- coef1@x[x_ind]
    }
    beta_row_name <- c("(Intercept)")
    for (i in 1:(length(beta_est)-1)){
      beta_row_name <- c(beta_row_name,paste("x",i,sep = ""))
    }
    beta_est <- matrix(beta_est,length(beta_est),1)
    rownames(beta_est) <- beta_row_name
  }else if(penalty == "SCAD"){
    fit_cv_ncvreg <- cv.ncvreg(sample_gaussian[,-1],sample_gaussian[,1],family="gaussian",nfolds=k,penalty="SCAD")
    fit_cv_lambda <- fit_cv_ncvreg$lambda.min
    coef1 <- coef(fit_cv_ncvreg,s = fit_cv_lambda)
    coef1_name <- names(coef1)
    beta_est <- matrix(coef1,length(coef1),1)
    rownames(beta_est) <- coef1_name
  }else if(penalty == "MCP"){
    fit_cv_ncvreg <- cv.ncvreg(sample_gaussian[,-1],sample_gaussian[,1],family="gaussian",nfolds=k,penalty="MCP")
    fit_cv_lambda <- fit_cv_ncvreg$lambda.min
    coef1 <- coef(fit_cv_ncvreg,s = fit_cv_lambda)
    coef1_name <- names(coef1)
    beta_est <- matrix(coef1,length(coef1),1)
    rownames(beta_est) <- coef1_name
  }else stop("penalty should be lasso, SCAD or MCP(beta_est_gaussian)")
  return(beta_est)
}

#beta_est_logistics: function to get the beta estimation for logistics dataset by our method in lasso and cross validation
#                  : get SCAD and MCP estimation by iteration
#                  : No intercept and No standardization in the glmnet() method
#input: sample_logstics(matrix): 
#       sample_complete(matrix): after deleting all missing value,used to generate logistics dataset when we cross validation
#       k: number of folders for cross validation
#       n_iter_SCAD: iteration time for SCAD, if 0, no SCAD will be estimated
#       n_iter_MCP: iteration time for MCP, if 0, no MCP will be estimated
#'      lasso_for_SCAD_count: count how many times use lasso estimation for SCAD
#'      lasso_for_SCAD_count: count how many times use lasso estimation for MCP
#'      beta_vector: true value of beta vector which can be used as initial value to calculate weights
#'      initial_true_indicator_SCAD: F: use the lasso estimatior as initial to calculate weights
#'                                   T: use the true beta value as initial to calculate weights
#'      initial_true_indicator_MCP: F: use the lasso estimatior as initial to calculate weights
#'                                  T: use the true beta value as initial to calculate weights
#'      lambda_location_SCAD: choose what lambda in the lambda path to use in the SCAD and MCP iteration method
#'      /lambda_location_MCP "all": use all of the lambda
#'                            any numeric vector(e.g.,1:30,20:50): use the location indicated in the numeric vector and will replace
#'                                                                 the maximum number of vector to the length of the lambda path if the
#'                                                                 maximum number of the vector is larger than the length of the lambda path
#output: a list include lasso, SCAD, MCP estimation

beta_est_logistics <- function(sample_logistic=sample_logistic,sample_complete=sample_complete,k=k,n_iter_SCAD=0,n_iter_MCP=0,
                               lasso_for_SCAD_count = 0, lasso_for_MCP_count = 0,
                               beta_vector,initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F,
                               lambda_location_SCAD="all",lambda_location_MCP="all"){
  
  #first, get the lasso estimation
  print("get path")
  lam_path1 <- glmnet(sample_logistic[,-1],factor(sample_logistic[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)$lambda
  print("end of getting path")
  #lam_path1 <- lam_path1[seq(1,length(lam_path1),by=3)]
  #get the lambda value we need
  lambda_value <- initial_universial_calculator(sample_original=sample_complete,sample_logistic=sample_logistic,k=k,
                                         lam_indicator=lam_path1,
                                         penalty_indicator="lasso")
  beta_lasso = beta_est(sample_logistic=sample_logistic,lambda=lambda_value$initial_lambda_value,penalty_indicator="lasso")
  print(beta_lasso)
  
  #second, get the SCAD estimation
  print("begein SCAD")
  SCAD_est <- beta_iteration_est(sample_logistic=sample_logistic,sample_complete=sample_complete,
                                 k=k,n_iteration=n_iter_SCAD,weight_fun=SCAD_iteration_weight,lambda_location=lambda_location_SCAD)
  print("end SCAD")
  
  #third, get the MCP estimation
  MCP_est <- beta_iteration_est(sample_logistic=sample_logistic,sample_complete=sample_complete,
                                 k=k,n_iteration=n_iter_MCP,weight_fun=MCP_iteration_weight,lambda_location=lambda_location_MCP)
  
  beta_all <- list(beta_lasso=beta_lasso,beta_SCAD=SCAD_est$beta,beta_MCP=MCP_est$beta,
                   lasso_for_SCAD_count=lasso_for_SCAD_count, lasso_for_MCP_count=lasso_for_MCP_count,
                   SCAD_lambda_vec=SCAD_est$lambda,MCP_lambda_vec=MCP_est$lambda,
                   SCAD_weight_maxtrix=SCAD_est$weight,MCP_weight_maxtrix=MCP_est$weight,
                   SCAD_lambda_location=lambda_location_SCAD,MCP_lambda_location=lambda_location_MCP)
  return(beta_all)
}


#' steps to get the target estimation(SCAD/MCP)in each iteration:
#' step1: get a lambda path from glmnet() method and the corresponding beta_estimation for each lambda in the lambda path
#' step2: use the lambda and corresponding beta_estimation to calculate the weight(a matrix, each row 
#'        is the a vector of weight related to one lambda for the lambda path)
#' step3: use each weight to get the new beta esimation(SCAD/MCP) and choose the best one(which the result for the current iteration) by cross validation
#' step4: use the estimation from step3 and lambda path to repeat step2 and step3 to get new estimation in the following estimation


#' beta_iteration_est: get beta estimation from iteratin method(for both SCAD and MCP, use different weight function)
#' input: sample_logstics(matrix): 
#'        sample_complete(matrix): after deleting all missing value,used to generate logistics dataset when we cross validation
#'        k: number of folders for cross validation
#'        n_iteration: iteration time, if 0, no estimation will be estimated
#'        weight_fun(function): function used to calculate weight
#'        lambda_location: choose what lambda in the lambda path to use in the SCAD and MCP iteration method
#'                         "all": use all of the lambda
#'                         any numeric vector(e.g.,1:30,20:50): use the location indicated in the numeric vector and will replace
#'                                                              the maximum number of vector to the length of the lambda path if the
#'                                                              maximum number of the vector is larger than the length of the lambda path
#' output(list): beta_iteration_matrix: each row is an estimation from one iteration, if n_iteration is 0, a one row matrix with all value are -99 will be generated
#'               weight_iteration_matrix: each row is the weight used in each iteration
#'               lambda_iteration_vector: each element is the lambda used in each iteration


beta_iteration_est <- function(sample_logistic=sample_logistic,sample_complete=sample_complete,
                               k=k,n_iteration=0,weight_fun,lambda_location="all"){
  #get the lasso model and lambda path
  print("begin get iteration path")
  lasso_fit <- glmnet(sample_logistic[,-1],factor(sample_logistic[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)
  print("end get iteration path")
  #generate lasso estimation matrix
  #this matrix has intercept column
  beta_lasso_matrix <- beta_lasso_model_lambda(lasso_fit)
  lambda_path_lasso <- lasso_fit$lambda
  if(paste(lambda_location,collapse = "") == "all"){
    lambda_path_ini <- lambda_path_lasso
    beta_lasso_matrix <- beta_lasso_matrix
    print("lambda ini for 'all' is: ")
    print(lambda_path_ini)
  }else if(all(is.numeric(lambda_location))){
    if(max(lambda_location)<=length(lambda_path_lasso)){
      lambda_path_ini <- lambda_path_lasso[lambda_location]
      beta_lasso_matrix <- beta_lasso_matrix[lambda_location,]
    }else{
      lambda_path_ini <- lambda_path_lasso[min(lambda_location):length(lambda_path_lasso)]
      beta_lasso_matrix <- beta_lasso_matrix[min(lambda_location):length(lambda_path_lasso),]
    }
  }else stop("lambda_location should be 'all' or a numeric vector(beta_iteration_est)")
  
  #iteration
  #' in each iteration,
  #' step1: use lambda path and corresponding beta matrix to generate weight matrix
  #' step2: delete the lambda and weight with weight 0
  #' step3: get the current_beta_est matrix from weighted lasso method 
  #' step4: find the best beta estimation by cross validation method and store it to the corresponding rows in beta_iteration_res matrix
  if (n_iteration==0){
    beta_iteration_res <- beta_lasso_matrix[1,]
    beta_iteration_res[1:length(beta_iteration_res)] <- -99
    weight_iteration_res <- beta_iteration_res[,-1]
    lambda_res <- NA
  }else if(n_iteration < 0){
    stop("n_iter should be non-negative(beta_est_logistics)")
  }else{
    #generate a matrix to store beta iteration result
    beta_iteration_res <- matrix(-99,n_iteration,dim(beta_lasso_matrix)[2])
    colnames(beta_iteration_res) <- colnames(beta_lasso_matrix)
    weight_iteration_res <- matrix(-99,n_iteration,dim(beta_lasso_matrix)[2]-1)
    colnames(weight_iteration_res) <- colnames(beta_lasso_matrix)[-1]
    lambda_iteration_res <- numeric(n_iteration)
    #begin iteration
    for(i in 1:n_iteration){
      if(i==1){
        #step1:
        print("begin first iteration")
        current_lambda_path <- lambda_path_ini
        current_beta_matrix <- beta_lasso_matrix
        #generate weight matrix
        #weight matrix should not have intercept column
        current_weight_matrix<- current_beta_matrix
        current_weight_matrix<- current_weight_matrix[,-1]
        current_weight_matrix[,] <- -99
        for(j in 1:length(current_lambda_path)){
          current_weight_matrix[j,] <- weight_fun(current_beta_matrix[j,][-1],current_lambda_path[j])
        }
        print("end step1")
        #step2:
        zero_test <- apply(current_weight_matrix != 0,1,sum)
        no_zero_weight_ind <- which(zero_test != 0)
        current_lambda_path <- current_lambda_path[no_zero_weight_ind]
        current_weight_matrix <- current_weight_matrix[no_zero_weight_ind,]
        print("end step2")
        #step3:
        #creat current_beta_est matrix
        current_beta_est <- current_beta_matrix[no_zero_weight_ind,]
        current_beta_est[,] <- -99
        for(j in 1:dim(current_weight_matrix)[1]){
          current_beta_est[j,] <- beta_est_weight(sample_logistic = sample_logistic,weights = current_weight_matrix[j,])
          print(j)
        }
        print("end step3")
        #step4:
        current_ind <- cv_weight_finder(sample_complete=sample_complete,sample_logistic=sample_logistic,k=k,weight_matrix=current_weight_matrix)
        print("the index choosed is")
        print(current_ind)
        #assign result
        if(dim(current_beta_est)[1]!=dim(current_weight_matrix)[1]) stop("row in current_beta_est and current_weight_matrix does not match(beta_iteration_est)")
        if(dim(current_beta_est)[1]!=length(current_lambda_path)) stop("row in current_beta_est and current_lambda_path does not match(beta_iteration_est)")
        
        beta_iteration_res[i,] <- current_beta_est[current_ind,]
        weight_iteration_res[i,] <- current_weight_matrix[current_ind,]
        lambda_iteration_res[i] <- current_lambda_path[current_ind]
        print("end step4")
        print("end first iteration")
      }else{
        #step1:
        current_lambda_path <- current_lambda_path
        current_beta_matrix <- current_beta_est
        #generate weight matrix
        #weight matrix should not have intercept column
        current_weight_matrix<- current_beta_matrix
        current_weight_matrix<- current_weight_matrix[,-1]
        current_weight_matrix[,] <- -99
        for(j in 1:length(current_lambda_path)){
          current_weight_matrix[j,] <- weight_fun(current_beta_matrix[j,][-1],current_lambda_path[j])
        }
        #step2:
        zero_test <- apply(current_weight_matrix != 0,1,sum)
        no_zero_weight_ind <- which(zero_test != 0)
        current_lambda_path <- current_lambda_path[no_zero_weight_ind]
        current_weight_matrix <- current_weight_matrix[no_zero_weight_ind,]
        #step3:
        #creat current_beta_est matrix
        current_beta_est <- current_beta_matrix[no_zero_weight_ind,]
        current_beta_est[,] <- -99
        for(j in 1:dim(current_weight_matrix)[1]){
          current_beta_est[j,] <- beta_est_weight(sample_logistic = sample_logistic,weights = current_weight_matrix[j,])
        }
        #step4:
        current_ind <- cv_weight_finder(sample_complete=sample_complete,sample_logistic=sample_logistic,k=k,weight_matrix=current_weight_matrix)
        print("the index choosed is")
        print(current_ind)
        #assign result
        if(dim(current_beta_est)[1]!=dim(current_weight_matrix)[1]) stop("row in current_beta_est and current_weight_matrix does not match(beta_iteration_est)")
        if(dim(current_beta_est)[1]!=length(current_lambda_path)) stop("row in current_beta_est and current_lambda_path does not match(beta_iteration_est)")
        
        beta_iteration_res[i,] <- current_beta_est[current_ind,]
        weight_iteration_res[i,] <- current_weight_matrix[current_ind,]
        lambda_iteration_res[i] <- current_lambda_path[current_ind]
      }
    }
  }
  result_list <- list(beta=beta_iteration_res,
                      weight=weight_iteration_res,
                      lambda=lambda_iteration_res)
  return(result_list)
}




#define SCAD iteration function to calculate the weight
SCAD_iteration_weight <- function(lasso_est,lambda){
  res <- numeric(length(lasso_est))
  lasso_est <- abs(lasso_est)
  for (i in 1:length(res)){
    if(lasso_est[i] <= lambda) {
      res[i] <- lambda
    }else{
      a=3.7
      num1 <- a*lambda - lasso_est[i]
      num1 <- positive_fun(num1)
      res[i] <- num1/(a-1)
    }
  }
  return(res)
}

#define MCP interation function to calculate the weight
MCP_iteration_weight <- function(lasso_est,lambda){
  a=3
  lasso_est <- abs(lasso_est)
  res <- lambda - lasso_est/a
  res <- positive_fun(res)
  return(res)
}

#' beta_lasso_model_lambda: generate beta based on lambda and glmnet() fitted lasso model, the output
#'                          is a matrix with same row as lenght of lambda
#' intput: lasso_model: a fitted lasso model
#' output(matrix): a matrix with all the beta estimation corresponding to the lambda path from input lasso model 


beta_lasso_model_lambda <- function(lasso_model){
  lambda_path <- lasso_model$lambda
  if(length(lambda_path)==0) stop ("lambda_path is empty.(beta_lasso_model_lambda,08.beta_guassian)")
  #generate beta_est_matrix
  temp_coef <- coef(lasso_model,s = lambda_path[1])
  beta_est_matrix <- matrix(-99,length(lambda_path),temp_coef@Dim[1])
  beta_est <- rep(0,temp_coef@Dim[1])
  x_ind <- 0
  for (i in temp_coef@i){
    x_ind <- x_ind +1
    beta_est[i+1] <- temp_coef@x[x_ind]
  }
  beta_row_name <- c("(Intercept)")
  for (i in 1:(length(beta_est)-1)){
    beta_row_name <- c(beta_row_name,paste("x",i,sep = ""))
  }
  colnames(beta_est_matrix) <- beta_row_name
  
  for(i in 1:length(lambda_path)){
    current_lambda <- lambda_path[i]
    current_coef <- coef(lasso_model,s = current_lambda)
    beta_est <- rep(0,current_coef@Dim[1])
    x_ind <- 0
    for (j in current_coef@i){
      x_ind <- x_ind +1
      beta_est[j+1] <- current_coef@x[x_ind]
    }
    beta_est_matrix[i,] <- beta_est
  }
  return(beta_est_matrix)
}


#'cv_beta_finder: this function is used to find the index from weight matrix have the largest cross-validation value
#'input: weight_matrix(matrix): a weight matrix, each row is one weight
#'       sample_complete(after deleting missing), sample_logistic, k(number of folder), 
#'ouput(scalar): the row number for which row of weight_matrix has the largest cross-validation value

cv_weight_finder <- function(sample_complete,sample_logistic,k,weight_matrix){
  
  # check k is in the correct range
  if(k<2 | k>dim(sample_complete)[1]) stop("k should be greater than 1 and less than the rows in the original data(after deleting missing)")
  
  # generate logistic list
  logistic_list <- cv_logistic_prepare(sample_original=sample_complete,k=k)
  
  position_ind <- -999
  
  #begin count time
  run_time <- proc.time()
  
  #genereate corresponding cv from our cross-validation method
  #create a vector to store all the cross-validation value
  cv_value_vector <- numeric(dim(weight_matrix)[1])
  
  for (i in 1:length(cv_value_vector)){
    cv_value_vector[i] <- cv_calculator_weight(weight=weight_matrix[i,],sample_original=sample_complete,logistic_list=logistic_list,k=k)
    print(paste("weight_",i,sep = ""))
  }
  position_ind <- which.max(cv_value_vector)
  
  #end count time 
  run_time <- proc.time() - run_time
  
  #check initial value
  if (position_ind == -999) stop("initial calculation(cv_weight_finder)")
  result <- position_ind
  return(result)
}

########################### functions shared by old and current method# ###########################

#' beta_est_weight: fucntion to get the beta estimation based on weight and from lasso method
#'                  beta estimation function can be used in cross validation method
#'                  beta estimation function can be used in iteration for SCAD and MCP
#' input: weight, logistic_list
#' ouput(vector): beta estimation
beta_est_weight <- function(sample_logistic,weights){
  #check weight has same dimension of x
  if (length(weights) != (dim(sample_logistic)[2]-1)) stop("weight are not equal to number of x(beta_est_weight)")
  
  lambda = sum(weights)/(dim(sample_logistic)[2]-1)
  #lambda = 1
  #print(lambda)
  #exclude = which(weights==0)
  
  fit_lasso<- glmnet(as.matrix(sample_logistic[,-1]),factor(sample_logistic[,1],levels=c(0,1)),family="binomial",lambda = lambda,intercept = F, standardize = F, penalty.factor=weights)
  coef1 <- coef(fit_lasso,s = lambda)
  beta_est <- rep(0,coef1@Dim[1])
  x_ind <- 0
  for (i in coef1@i){
    x_ind <- x_ind +1
    beta_est[i+1] <- coef1@x[x_ind]
  }
  beta_row_name <- c("(Intercept)")
  for (i in 1:(length(beta_est)-1)){
    beta_row_name <- c(beta_row_name,paste("x",i,sep = ""))
  }
  beta_est <- matrix(beta_est,length(beta_est),1)
  rownames(beta_est) <- beta_row_name
  colnames(beta_est) <- lambda
  return(beta_est)
}

#' positive_fun: if the input is postive then return it, if input less than or equal to 0, return 0
#' input(scale or vector): x: numeric value
#' ouput(sacle or vecotr, same as input): postive number or zero
positive_fun <- function(x){
  len = length(x)
  res <- numeric(len)
  for(i in 1:len){
    if (x[i]>0) res[i] <- x[i]
    else res[i] <- 0
  }
  return(res)
}

############################################
# this function is based on function cv_calculator from 07.cv_esimator.R
# cv_calculator_weight: calculate cv values(our method) based on the input weight(a vector)
# input: weight, logistic_list,sample_original, k
# ouput: a single cv_value corresponding to input weight
cv_calculator_weight <- function(weight,sample_original=sample_original,logistic_list=logistic_list,k=k){
  #generate a list to store the obs number be removed in cross validation
  dim_module <- dim(sample_original)[1] %% k
  obs_length <-dim(sample_original)[1] %/% k
  obs_list <- list() 
  for(i in 1:k){
    if (i <= dim_module) obs_list[[i]] <- ((i-1)*obs_length + (i-1)+1):(i*obs_length + i)
    else obs_list[[i]] <- (obs_length * dim_module + dim_module + obs_length*(i-dim_module-1)+1):
        (obs_length * dim_module + dim_module + obs_length*(i-dim_module))
  }
  if(length(logistic_list) != k) stop("logistics list does not match k cv_calculator")
  
  #calculate the cv value
  result <- matrix(0,1,2)
  colnames(result) <- c("lambda","cv_value")
  cv_vec <- rep(0,k)
  for (j in 1:k){
    beta_est_k <- beta_est_weight(weights = weight,sample_logistic=logistic_list[[j]])
    print(beta_est_k)
    ll <- loglikelihood(sample_original[obs_list[[j]],],beta_est_k)
    #ll_k <- loglikelihood(sample_original[-obs_list[[j]],],beta_est_k)
    print(ll)
    print(dim(sample_original[obs_list[[j]],]))
    #print(ll_k)
    print(obs_list[[j]])
    cv_vec[j] <- ll
    print(cv_vec)
  }
  print(paste("result",result))
  result[1,1] <- 0
  result[1,2] <- sum(cv_vec)
  print(paste("result",result))
  return(result[1,2])
}







#################################for old method######################################




#'lambda_path_generator: a function to generate the lambda path which is used to caculate the weight path,
#'                       to prevent all the penality are zero, if the beta_min > a*lambda, the path will
#'                       generate between beta_max and beta_min/a*1.01, else the path is begin with a 
#'                       initial value beta_max and end with beta_max*ratio, and then are divided 
#'                       to n point under log-scale equal distance
#'input: beta_max: it is the maximum element of beta estimation vector
#'       beta_min: it is the minimum element of beta estimation vector
#'       a: the a from SCAD and MCP(for SCAD a=3.7, for MCP a=3 at default)
#'       ratio: the coefficient to decide the end of lambda
#'       n: the number of data point in the lambda path
#'output(vector): the lambda path

lambda_path_generator <- function(beta_max,beta_min,a,ratio,n){
  if (beta_min == 0){
    lambda_vec <- c(beta_max,beta_max*ratio)
  }else{
    if (beta_min>a*beta_max*ratio){
      lambda_vec <- c(beta_max,beta_min/a*1.01)
    }else{
      lambda_vec <- c(beta_max,beta_max*ratio)
    }
  }
  lambda_vec <- log(lambda_vec)
  lambda_vec <- seq(lambda_vec[1],lambda_vec[2],length=n)
  lambda_vec <- exp(lambda_vec)
  return(lambda_vec)
}


########################################################
# this function is based on function initial_universial_calculator from 07.cv_esimator.R
# cv_weight_lambda_finder: function to find best weight for SCAD or MCP(weight is a function of lambda) in one iteration
#                          parameter lasso_est and lam_path should be updated each iteration
# input: sample_original(after deleting missing), sample_logistic, k(number of folder), 
#        lam_path: the input SHOULD be vector of lambda path which is generated from lambda_path_generator,
#        weight_fun: a function is used to calculate weight(SCAD or MCP)
#        lasso_est: an estimation from previous iteration to generate weight
# ouput(list):lambda(corresponding to cv), corresponding cv value, time used,
#        lambda_path, corresponding cv_path

cv_weight_lambda_finder <- function(sample_original=sample_original,sample_logistic=sample_logistic,
                                    k=k, lam_path=-99, weight_fun,lasso_est){
  #' to find best lambda to generate the weight:
  #' choose the lambda give the minimum cv value(from our method) from the vector
  #' output: list - inital value, initial cv value, time cost, lambda path, cv(our method) path, method used(text, inidcator), penalty type 

  # check k is in the correct range
  if(k<2 | k>dim(sample_original)[1]) stop("k should be greater than 1 and less than the rows in the original data(after deleting missing)")
  
  # generate logistic list
  logistic_list <- cv_logistic_prepare(sample_original=sample_original,k=k)
  
  initial_value <- -999
  
  #begin count time
  run_time <- proc.time()

  #genereate corresponding cv from our method
  if(length(lam_path)>=1){
    #create a matrix to store lambda value and corresponding cv value(our method)
    path_matrix <- matrix(-99,length(lam_path),2)
    colnames(path_matrix) <- c("lambda","cv_value")
    for (i in 1:length(lam_path)){
      path_matrix[i,1] <- lam_path[i]
      current_lambda_weight <- weight_fun(lasso_est=lasso_est[-1],lambda=lam_path[i])
      path_matrix[i,2] <- cv_calculator_weight(weight=current_lambda_weight,sample_original=sample_original,logistic_list=logistic_list,k=k)
      initial_value <- path_matrix[,1][which.max(path_matrix[,2])]
      min_cv_value <- max(path_matrix[,2])
      print(paste("lambda","_",i,sep = ))
    }
  }else stop("lambda path is wrong initial_universial_calculator")
  
  #end count time 
  run_time <- proc.time() - run_time
  
  #check initial value
  if (initial_value == -999) stop("initial calculation wrong initial_universial_calculator")
  result <- list(initial_lambda_value=initial_value,cv_value=min_cv_value,time_cost_initial=run_time,
                 lambda_path=lam_path,lambda_path_cv=path_matrix[,2])
  return(result)
}







