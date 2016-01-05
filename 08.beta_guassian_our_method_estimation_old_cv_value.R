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
#output: a list include lasso, SCAD, MCP estimation

beta_est_logistics <- function(sample_logistic=sample_logistic,sample_complete=sample_complete,k=k,n_iter_SCAD=0,n_iter_MCP=0,
                               lasso_for_SCAD_count = 0, lasso_for_MCP_count = 0,
                               beta_vector,initial_true_indicator_SCAD=F,initial_true_indicator_MCP=F){
  
  #first, get the lasso estimation
  lam_path1 <- glmnet(sample_logistic[,-1],factor(sample_logistic[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)$lambda
  #lam_path1 <- lam_path1[seq(1,length(lam_path1),by=3)]
  #get the lambda value we need
  lambda_value <- initial_universial_calculator(sample_original=sample_complete,sample_logistic=sample_logistic,k=k,
                                         lam_indicator=lam_path1,
                                         penalty_indicator="lasso")
  beta_lasso = beta_est(sample_logistic=sample_logistic,lambda=lambda_value$initial_lambda_value,penalty_indicator="lasso")
  print(beta_lasso)
  
  #define a positve function
  positive_fun <- function(x){
    len = length(x)
    res <- numeric(len)
    for(i in 1:len){
      if (x[i]>0) res[i] <- x[i]
      else res[i] <- 0
    }
    return(res)
  }

  #second, get the SCAD estimation
  SCAD_lambda_vec <- numeric(n_iter_SCAD)
  SCAD_weight_maxtrix <- matrix(-99,n_iter_SCAD,length(beta_lasso)-1)
  if (n_iter_SCAD==0){
    beta_SCAD=0
  }else if(n_iter_SCAD < 0){
    stop("n_iter_SCAD should be non-negative(beta_est_logistics)")
  }else{
    #SCAD restult matrix, each row is one result from one iteration
    beta_SCAD <- matrix(0,n_iter_SCAD,length(beta_lasso))
    colnames(beta_SCAD) <- rownames(beta_lasso)
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
    for (i in 1:n_iter_SCAD){
      if (i==1){
        if(initial_true_indicator_SCAD){
          initial_weight <- c(0,beta_vector)
        }else{
          initial_weight <- beta_lasso
        }
        #find the lambda
        beta_max <- max(abs(initial_weight[-1]))
        beta_min <- min(abs(initial_weight[-1]))
        lambda_path_weight <- lambda_path_generator(beta_max=beta_max,beta_min=beta_min,a=3.7,ratio=0.001,n=30)
        cv_lambda <- cv_weight_lambda_finder(sample_original=sample_complete,sample_logistic=sample_logistic,
                                             k=k, lam_path=lambda_path_weight, weight_fun=SCAD_iteration_weight,lasso_est=initial_weight)$initial_lambda_value
        
        
        SCAD_lambda_vec[i] <- cv_lambda
        #calculate the weight
        weights = SCAD_iteration_weight(initial_weight,cv_lambda)
        weights = weights[-1]
        SCAD_weight_maxtrix[i,] <- weights
        print(weights)
        if (sum(weights)==0){
          beta_SCAD[i,] <- beta_lasso
          lasso_for_SCAD_count = lasso_for_SCAD_count + 1
        }else{
          beta_SCAD[i,] <- beta_est_weight(sample_logistic, weights)
        }
      }else{
        #find the lambda
        beta_max <- max(abs(beta_SCAD[i-1,][-1]))
        beta_min <- min(abs(beta_SCAD[i-1,][-1]))
        lambda_path_weight <- lambda_path_generator(beta_max=beta_max,beta_min=beta_min,a=3.7,ratio=0.001,n=30)
        cv_lambda <- cv_weight_lambda_finder(sample_original=sample_complete,sample_logistic=sample_logistic,
                                             k=k, lam_path=lambda_path_weight, weight_fun=SCAD_iteration_weight,lasso_est=beta_SCAD[i-1,])$initial_lambda_value
        SCAD_lambda_vec[i] <- cv_lambda
        #calculate the weight
        weights = SCAD_iteration_weight(beta_SCAD[i-1,],cv_lambda)
        weights = weights[-1]
        SCAD_weight_maxtrix[i,] <- weights
        print(weights)
        if (sum(weights)==0){
          beta_SCAD[i,] <- beta_lasso
        }else{
          beta_SCAD[i,] <- beta_est_weight(sample_logistic, weights)
        }
      }
    }
  }
  
  #third, get the MCP estimation
  MCP_lambda_vec <- numeric(n_iter_MCP)
  MCP_weight_maxtrix <- matrix(-99,n_iter_MCP,length(beta_lasso)-1)
  if (n_iter_MCP==0){
    beta_MCP=0
  }else if(n_iter_MCP < 0){
    stop("n_iter_MCP should be non-negative(beta_est_logistics)")
  }else{
    #MCP restult matrix, each row is one result from one iteration
    beta_MCP <- matrix(0,n_iter_MCP,length(beta_lasso))
    colnames(beta_MCP) <- rownames(beta_lasso)
    #define MCP interation function to calculate the weight
    MCP_iteration_weight <- function(lasso_est,lambda){
      a=3
      lasso_est <- abs(lasso_est)
      res <- lambda - lasso_est/a
      res <- positive_fun(res)
      return(res)
    }
    for (i in 1:n_iter_MCP){
      if (i==1){
        if(initial_true_indicator_MCP){
          initial_weight <- c(0,beta_vector)
        }else{
          initial_weight <- beta_lasso
        }
        #find the lambda
        beta_max <- max(abs(initial_weight[-1]))
        beta_min <- min(abs(initial_weight[-1]))
        lambda_path_weight <- lambda_path_generator(beta_max=beta_max,beta_min=beta_min,a=3,ratio=0.001,n=30)
        cv_lambda <- cv_weight_lambda_finder(sample_original=sample_complete,sample_logistic=sample_logistic,
                                             k=k, lam_path=lambda_path_weight, weight_fun=MCP_iteration_weight,lasso_est=initial_weight)$initial_lambda_value
        MCP_lambda_vec[i] <- cv_lambda 
        #calculate the weight
        weights = MCP_iteration_weight(initial_weight,cv_lambda)
        weights = weights[-1]
        MCP_weight_maxtrix[i,] <- weights
        print(weights)
        if (sum(weights)==0){
          beta_MCP[i,] <- beta_lasso
          lasso_for_MCP_count = lasso_for_MCP_count + 1
        }else{
          beta_MCP[i,] <- beta_est_weight(sample_logistic, weights)
        }
      }else{
        #find the lambda
        beta_max <- max(abs(beta_MCP[i-1,][-1]))
        beta_min <- min(abs(beta_MCP[i-1,][-1]))
        lambda_path_weight <- lambda_path_generator(beta_max=beta_max,beta_min=beta_min,a=3,ratio=0.001,n=30)
        cv_lambda <- cv_weight_lambda_finder(sample_original=sample_complete,sample_logistic=sample_logistic,
                                             k=k, lam_path=lambda_path_weight, weight_fun=MCP_iteration_weight,lasso_est=beta_MCP[i-1,])$initial_lambda_value
        
        MCP_lambda_vec[i] <- cv_lambda 
        #calculate the weight
        weights = MCP_iteration_weight(beta_MCP[i-1,],cv_lambda)
        weights = weights[-1]
        MCP_weight_maxtrix[i,] <- weights
        print(weights)
        if (sum(weights)==0){
          beta_MCP[i,] <- beta_lasso
        }else{
          beta_MCP[i,] <- beta_est_weight(sample_logistic, weights)
        }
      }
    }
  }
  
  beta_all <- list(beta_lasso=beta_lasso,beta_SCAD=beta_SCAD,beta_MCP=beta_MCP,
                   lasso_for_SCAD_count=lasso_for_SCAD_count, lasso_for_MCP_count=lasso_for_MCP_count,
                   SCAD_lambda_vec=SCAD_lambda_vec,MCP_lambda_vec=MCP_lambda_vec,
                   SCAD_weight_maxtrix=SCAD_weight_maxtrix,MCP_weight_maxtrix=MCP_weight_maxtrix)
  return(beta_all)
}


#' steps to get the best lambda to calculate weight in each iteration:
#' step1: get the maximum value from beta estimator vector
#' step2: use the value from step1 and function lambda_path_generator to get a path of lambda
#' step3: use the function cv_weight_lambda_finder to get the best lambda which has the maximum cross-validation value
#' step4: use the lambda got from step3 to calculate the weight

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





