# cross validation in original dataset
# cv_original take the dataset and generate the beta estimation
# use nlm() to get the estimation of lambda
# input: dataset(original(after deleting missing value), logistics), k(>=2),nonzero num of parameter
#        list of intial lambda(lam_ini_lasso,lam_ini_SCAD,lam_ini_MCP,defaul is -99 which means we get lambda path from the package)
# output: best lambda by cross validation with three values(lasso, SCAD, MCP)，beta_est, method used to choose initial lambda for nlm(),
#         time used to get the estimation

#check package
if(!require("glmnet")){
  install.packages("glmnet")
  library(glmnet)
}
if(!require("ncvreg")){
  install.packages("ncvreg")
  library(ncvreg)
}

########################################################
#' define a function to estimte the lambda(our method) based on nlm() or nlminb()
#' input: data_original(after deleting missing), sample_logistic, k(number of folder),
#'        lam_indicator, penalty_indicator, method_indicator(nlm() or nlminb())
#' output: list - estimation of lambda
lambda_estimator <- function(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                             lam_indicator=-99,
                             penalty_indicator=penalty_indicator,
                             minimal_method){
  #' lminimal_method = nlm
  #'               = nlminb
  
  lambda_est <- -999
  
  #begin count time
  run_time <- proc.time()
  
  
  #get initial value
  lambda_ini <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                                              lam_indicator=lam_indicator,
                                              penalty_indicator=penalty_indicator)$initial_lambda_value
  if (minimal_method=="nlm"){
    
  }else if(minimal_method=="nlminb"){
    
  }else stop("lambda_method input is wrong lambda_estimator")
  
  #end count time 
  run_time <- proc.time() - run_time
  
  #check the estimation
  if (lambda_est == -999) stop("lambda estimation is wrong lambda_estimator")
  
  result <- list(lambda_estimation = lambda,estimation_time_using = run_time,
                 method_used = minimal_method,lambda_initial=lambda_ini)
  
}



########################################################
# define a universial initial calculator
# function to find initial value of lambda for nlm()
# input: sample_original(after deleting missing), sample_logistic, k(number of folder), lam_indicator, penalty_indicator
# ouput: a list-initial value, corresponding cv value, time used, method used and numeric indicator
#        lambda_path, corresponding cv_path, penalty type

initial_universial_calculator <- function(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                                          lam_indicator=-99,
                                          penalty_indicator=penalty_indicator){
  #' to calculate initial value for lasso, SCAD, MCP based on the penalty_indicator
  #' two step:
  #' first, use package or user input to get a path of lambda, it can be a single value or vector
  #' second, if the path of lambda is a vecotr, choose the lambda give the minimum cv value(from our method) from the vector
  #'         if the path of lambda is a scalar, just return it
  #' output: list - inital value, initial cv value, time cost, lambda path, cv(our method) path, method used(text, inidcator), penalty type 
  
  # lam_indicator : define how to choose initial value of lambda
  #               = -99: the initial value is the lambda give the minimum cv value(from our method) from the path of lambda got from the package
  #               = -98: use the lambda got from cross validation provided by the package
  #               = vector: find a lambda which is from this vector gives the minimum cv value(from our method)
  #               = scalar: use this value as the initial value
  
  #' text message for initial method
  #' ini_method_ind: 1: lambda path from package
  #'                 2: lambda from package cross validation
  #'                 3: user input vector
  #'                 4: user input scalar
  #'                -1: something wrong
  
  # check k is in the correct range
  if(k<2 | k>dim(sample_original)[1]) stop("k should be greater than 1 and less than the rows in the original data(after deleting missing)")
  
  # generate logistic list
  logistic_list <- cv_logistic_prepare(sample_original=sample_original,k=k)
  
  ini_method <- "ini_method"
  ini_method_ind <- -1
  
  initial_value <- -999
  
  #begin count time
  run_time <- proc.time()
  
  #first, generate lambda path
  if (length(lam_indicator) > 1){
    ini_method <- "user input vector"
    ini_method_ind <- 3
    lam_path <- lam_indicator
  }else{if (lam_indicator == -99){
    ini_method <- "lambda path from package"
    ini_method_ind <- 1
    #decided the intercept
    if (penalty_indicator == "lasso"){
      lam_path <- glmnet(as.matrix(sample_logistics[,-1]),factor(sample_logistics[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)$lambda
    } else if (penalty_indicator == "SCAD"){
      lam_path <- ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="SCAD")$lambda
    } else if (penalty_indicator == "MCP"){
      lam_path <- ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="MCP")$lambda
    } else stop(" penalty_indicator is wrong")
  }else if (lam_indicator == -98){
    ini_method <- "lambda from package cross validation"
    ini_method_ind <- 2
    if (penalty_indicator == "lasso"){
      lam_path <- cv.glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial",type.measure = "class",intercept = F,standardize = F)$lambda.min
    } else if (penalty_indicator == "SCAD"){
      lam_path <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="SCAD")$lambda.min
    } else if (penalty_indicator == "MCP"){
      lam_path <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="MCP")$lambda.min
    } else stop(" penalty_indicator is wrong initial_universial_calculator")
  }else if (length(lam_indicator) == 1){
    ini_method <- "user input scalar"
    ini_method_ind <- 4
    lam_path <- lam_indicator
  }else stop("lam_indicator input wrong initial_universial_calculator")
  }
  
  #second, choose initial lambda
  #genereate corresponding cv from our method
  #define a function to generate cv from our method cv_calculator
  if(length(lam_path)>=1){
    #create a matrix to store lambda value and corresponding cv value(our method)
    path_matrix <- matrix(-99,length(lam_path),2)
    colnames(path_matrix) <- c(paste("lambda_",penalty_indicator,sep = ""),"cv_value")
    for (i in 1:length(lam_path)){
      path_matrix[i,1] <- lam_path[i]
      path_matrix[i,2] <- cv_calculator(lambda=lam_path[i],sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator=penalty_indicator)
      initial_value <- path_matrix[,1][which.max(path_matrix[,2])]
      min_cv_value <- max(path_matrix[,2])
      print(paste(penalty_indicator,"_",i,sep = ))
    }
  }else stop("lambda path is wrong initial_universial_calculator")
  
  #end count time 
  run_time <- proc.time() - run_time
  
  #check initial value
  if (initial_value == -999) stop("initial calculation wrong initial_universial_calculator")
  result <- list(initial_lambda_value=initial_value,cv_value=min_cv_value,time_cost_initial=run_time,
                 method=ini_method,method_ind=ini_method_ind,
                 lambda_path=lam_path,lambda_path_cv=path_matrix[,2],
                 penalty=penalty_indicator)
  return(result)
}

########################################################
#function to calculate the log-pseudolikelihood
#input:dataset,beta_estimation
#output: likelihood value
loglikelihood <- function(dataset,beta_estimation){
  obs_length <- dim(dataset)[1]
  dataset_y <- dataset[,1]
  #wihtout 1 when there is no intercept
  dataset_x <- cbind(1,dataset[,-1])
  #   print(head(dataset_y))
  #   print(head(dataset_x))
  #   beta_estimation <- beta_est
  #   dataset <- sample_original
  #   i <- 5
  #   j <- 10
  
  count <- 0
  loglikelihood <- 0
  for (j in 1:obs_length){
    for(i in 1:obs_length){
      if (i<j){
        if(length(as.vector(dataset_x[i,]-dataset_x[j,])) != length(as.vector(beta_estimation))) stop("x and beta_est does not match loglikelihood")
        loglikelihood <- loglikelihood + (-log(1+exp(-(dataset_y[i]-dataset_y[j])*sum(as.vector(beta_estimation)*as.vector(dataset_x[i,]-dataset_x[j,])))))
        count <- count +1
      }
    }
  }
  #print(count)
  loglikelihood <- loglikelihood/count
  return(loglikelihood)
}

########################################################
#function used to get beta estimation
#input: logistics dataset, singe lambda value, penalty indicator(lasso, SCAD, MCP)
#output: estimaiton of beta
beta_est <- function(sample_logistic,lambda,penalty_indicator){
  if (penalty_indicator == "lasso"){
    #decided intercept
    fit_lasso<- glmnet(as.matrix(sample_logistic[,-1]),factor(sample_logistic[,1],levels=c(0,1)),family="binomial",lambda = lambda,intercept = F, standardize = F)
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
  }else if(penalty_indicator == "SCAD"){
    fit_SCAD <- ncvreg(as.matrix(sample_logistic[,-1]),sample_logistic[,1],family="binomial",penalty="SCAD",lambda = lambda)
    beta_est <- fit_SCAD$beta
    return(beta_est)
  }else if(penalty_indicator == "MCP"){
    fit_MCP <- ncvreg(as.matrix(sample_logistic[,-1]),sample_logistic[,1],family="binomial",penalty="MCP",lambda = lambda)
    beta_est <- fit_MCP$beta
    return(beta_est)
  }else stop(" penalty_indicator is wrong beta_est")
}

############################################
#logistics data generator: get the logistics dataset from original dataset
#input:original dataset
#output:logistics dataset
logistic_generator <- function(data1){
  #covert to logistic regression data
  logistic_sample <- matrix(0, choose(dim(data1)[1],2), dim(data1)[2])
  n_sample1 <- dim(data1)[1]
  row_count <- 1
  for(j in 1:n_sample1){
    for(i in 1:n_sample1){
      if (i<j){
        sgn1 <- -(data1[i,1]-data1[j,1])
        xval <- as.matrix((data1[i,-1]-data1[j,-1])*abs(data1[i,1]-data1[j,1]))
        if (sgn1>0){
          obs_temp <- c(0,xval)
          logistic_sample[row_count,] <- obs_temp
        }else if(sgn1<0){
          obs_temp <- c(1,xval)
          logistic_sample[row_count,] <- obs_temp
        }else if(sgn1 == 0){
          # delete after dataset generated
          obs_temp <- c(-1,xval)
          logistic_sample[row_count,] <- obs_temp
        }
        row_count = row_count + 1
      }
    }
  }
  
  logistic_sample <- as.data.frame(logistic_sample)
  colnames(logistic_sample) <- colnames(data1)
  # delete sgn = 0
  logistic_sample <- subset(logistic_sample,y!=-1)
  #head(logistic_sample)
  #tail(logistic_sample)
  return(logistic_sample)
}

############################################
#cv_logistic_prepare: generate the logistic dataset in a list for cv
#the purpose of this function is save time by avoiding generate logistic list every time
#input: sample_original,k
#output: a list with all logistic dataset for cross valiadation(our method)

cv_logistic_prepare <- function(sample_original=sample_original,k=k){
  #generate a list to store the obs number be removed in cross validation
  dim_module <- dim(sample_original)[1] %% k
  obs_length <-dim(sample_original)[1] %/% k
  obs_list <- list() 
  for(i in 1:k){
    if (i <= dim_module) obs_list[[i]] <- ((i-1)*obs_length + (i-1)+1):(i*obs_length + i)
    else obs_list[[i]] <- (obs_length * dim_module + dim_module + obs_length*(i-dim_module-1)+1):
        (obs_length * dim_module + dim_module + obs_length*(i-dim_module))
  }
  
  #generate the logistic dataset in a list
  logistic_list <- list()
  for (i in 1:k){
    data_k <- sample_original[-obs_list[[i]],]
    logistic_list[[i]] <- logistic_generator(data_k)
  }
  return(logistic_list)
}
#get the logistic list data
#logistic_list <- cv_logistic_prepare(sample_original=sample_original,k=k)

############################################
#cv_calculator: calculate cv values(our method) based on the input lambda(scalar)
#input: lambda, sample_original, k, penalty_indicator
#ouput: a single cv_value corresponding to lambda and penalty_indicator
cv_calculator <- function(lambda,sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator=penalty_indicator){
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
  if (penalty_indicator %in% c("lasso","SCAD","MCP")){
    #lasso
    result <- matrix(0,1,2)
    colnames(result) <- c(paste("lambda_",penalty_indicator,sep = ""),"cv_value")
    cv_vec <- rep(0,k)
    lambda <- lambda
    for (j in 1:k){
      beta_est_k <- beta_est(sample_logistic=logistic_list[[j]],lambda=lambda,penalty_indicator=penalty_indicator)
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
    result[1,1] <- lambda
    result[1,2] <- sum(cv_vec)
    print(paste("result",result))
  }else stop("penalty_indicator is wrong cv_calculator")
  return(result[1,2])
}







