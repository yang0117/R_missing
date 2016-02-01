#' 01.01. gamma caculator
#' 
#' 
#' gamma_cal: this function is used to get a gamma value based on the input loss rate.
#'            This function is from sample_generator1, and comment out the logistical data part
#'            Usage: every time using this function will get a value of gamma, and we can run 1000 times
#'                   and get an average of gamma to use in our function
#' input: n(sample_size),beta_vector,intercept,gamma(input gamma by hand,will be ignored if gamma_estimator_switch=T)
#'        gamma_estimator_switch (T=estimate gamma and ignore the gamma input by hand, F= use the gamma input by hand)
#'        loss_rate(percentage the sample is not missing), 
#'        method_indicator: only work for normal distribution
#'        ("exp":use the exp function, "extreme_min": remove miminum y, "extreme_max": remove maximum y, 
#'        "positive":keep 50% postive and 25% negative value of y)
#'        error_var: variance for error term
#'        y_logistic: F: generate y as normal distribution
#'                    T: generate y as logistic regression, it will ignore the value of method_indicator and will keep
#'                       all 1 of y and 25% 0 of y
#' output: would be a list, include dataset with missing and dataset for simulation(convert to logistic dataset)
#'         y_logistic, dataset, logistics_sample, method_indicator





if(!require("MASS")){
  library(MASS)
}

gamma_cal <- function(n=100, beta_vector=c(1.5, 2, 3, rep(0,5)), intercept=99,
                              gamma=-999, gamma_estimator_switch=T,
                              loss_rate=0.8,
                              method_indicator="exp",
                              error_var=1,
                              y_logistic=F,
                      x_missing_location=1,
                      error_independent=F){
  # #number of covariate
  # p <- 50
  # #number of obs
  # n <- 100
  # #other parameters
  # 
  # loss_rate <- 0.8
  # beta_vector <- c(0.5, 0.6, 0.7, 0.8, 0.9, rep(0,45))
  # 
  # gamma is the lost rate
  # gamma_estimator_switch <- T
  # 
  print("!!! important: This version has been modified to test scenario, DO NOT use this version before you confirm everything.
        Many parameter does not mean what they are in the original version(method_indicator).
        Added 'xy' method.")
  p=length(beta_vector)
  #covariance matrix of covariate
  x_sigma <- matrix(0,p,p)
  if(error_independent==F){
    for (i in 1:p){
      for (j in 1:p){
        x_sigma[i,j] <- 0.5^abs(i-j)
      }
    }
  }else{
    diag(x_sigma) <- 1
  }
  #x_sigma[1:10,1:10]
  #generate covariable matrix
  x <- mvrnorm(n,rep(0,p),x_sigma)
  
  
  
  if (y_logistic == T){
    #generate logistics sample and missing indicator
    if(logistic_method == "regular"){
      x <- x
    }else if(logistic_method == "Fan_2001"){
      # change last two x to rbinom(1,1,0.5)
      for(i in c(length(beta_vector),length(beta_vector)-1)){
        x_p <- rbinom(n,1,0.5)
        x[,i] <- x_p
      }
    }else stop("logistic_method input for logistic should be one of regular, Fan_2001.(sample_generator1)")
    #get the probability vecotr
    x_beta <- x %*% beta_vector + intercept
    p_vector <- exp(x_beta)/(1+exp(x_beta))
    if (length(p_vector) != n) stop("logistic: length of p_vector is not n (sample_generator)")
    y <- rbinom(n,1, p_vector)
    
    #' to generate the missing indicator
    #' missing method:
    #' y_single: only using y
    #' xy: use both x and y
    
    if (method_indicator == "y_single"){
      missing_binary <- ifelse(y==1,1,rbinom(n,1,0.25))
      gamma1 <- -999
    }else if(method_indicator == "xy"){
      gamma1 <- gamma
      if (gamma_estimator_switch){
        
        
      }
    }else stop("method_indicator input for logistic should be one of y_single, xy.(sample_generator1)")
  }else{
    #generate normal distribution sample and missing indicator
    #generate response vector
    #add error
    error_vector <- rnorm(n,0,error_var)
    x_error <- cbind(x,error_vector)
    y <- x_error %*% c(beta_vector,1) + intercept
    gamma1 <- gamma
    
    if(method_indicator %in% c("exp", "xy")){
      #estimate gamma
      #define missing function
      if (method_indicator == "exp"){
        missing_function <- function(gamma){
          res <- exp(-0.1*abs(y))
          res <- res*exp(-gamma*abs(x[,x_missing_location]))
          return(res)
        }
      }else if (method_indicator == "xy"){
        missing_function <- function(gamma){
          res <- ifelse(y>-0.9*sd(y)+intercept,1,0)
          res <- res* ifelse(x[,x_missing_location]>gamma,1,0)
          return(res)
        }
      }
      
      if(gamma_estimator_switch){
        gamma_eq <- function(gamma){
          #res <- sum(exp(gamma*y)/(1+exp(gamma*y)))/n
          #res <- sum(1/(1+exp(-gamma*y)))/n
          #res <- sum(exp(-gamma*abs(y)))/n-loss_rate
          res <- sum(missing_function(gamma))/n - loss_rate
          return(res)
        }
        
        # gamma_eq2 <- function(gamma){
        #   res <- rbinom(n,1,1/(1+exp(-gamma*y)))
        #   res <- sum(res)/n
        #   return(res)
        # }
        if (method_indicator == "exp"){
          gamma1 <- uniroot(gamma_eq,c(0,10))$root
        }else if (method_indicator == "xy"){
#           gamma_v1 <- seq(-10,10,by=0.01)
#           loss_v1 <- numeric(length(gamma_v1))
#           for(i in 1:length(gamma_v1)){
#             #loss_v1[i] <- gamma_eq(gamma_v1[i])+loss_rate
#             loss_v1[i] <- mean(y>gamma_v1[i])
#           }
#           #print(loss_v1)
#           plot(gamma_v1,loss_v1,type = "l")
          gamma1 <- uniroot(gamma_eq,c(-10,10))$root
        }
        
        #test code
        #   gammatest <- seq(0,1,length=1000)
        #   ytest <- sapply(gammatest, gamma_eq)
        #   plot(gammatest,ytest,type = "l")
        #   ytest  
      }
      # 1 means keep, 0 means missing
      missing_binary <- rbinom(n,1,missing_function(gamma1))
    }else if (method_indicator == "extreme_min"){
      missing_binary <- rep(1,n)
      missing_index <- order(y)[1:round(n*(1-loss_rate))]
      missing_binary[missing_index] <- 0
    }else if (method_indicator == "extreme_max"){
      missing_binary <- rep(1,n)
      missing_index <- order(y,decreasing = T)[1:round(n*(1-loss_rate))]
      missing_binary[missing_index] <- 0
    }else if (method_indicator == "positive"){
      missing_binary <- ifelse(y>0,1,rbinom(n,1,0.25))
    }else stop("method_indicator input should be one of exp, extreme_min, extreme_max(sample_generator1) or positive")
  }
  
  
  # make final dataset
  x_name <- character(p)
  for (i in 1:p) x_name[i] <- paste("x",i,sep = "")
  var_name <- c("missing_indicator", "y", x_name)
  final_data <- cbind(missing_binary,y,x)
  colnames(final_data) <- var_name
  final_data <- as.data.frame(final_data)
  
  
#   # for logistic dataset
#   #delete missing observations
#   sample1_no_missing <- subset(final_data,missing_indicator == 1)[,-1]
#   
#   #covert to logistic regression data
#   logistic_sample <- matrix(0, choose(dim(sample1_no_missing)[1],2), dim(sample1_no_missing)[2])
#   n_sample1 <- dim(sample1_no_missing)[1]
#   row_count <- 1
#   for(j in 1:n_sample1){
#     for(i in 1:n_sample1){
#       if (i<j){
#         sgn1 <- -(sample1_no_missing[i,1]-sample1_no_missing[j,1])
#         xval <- as.matrix((sample1_no_missing[i,-1]-sample1_no_missing[j,-1])*abs(sample1_no_missing[i,1]-sample1_no_missing[j,1]))
#         if (sgn1>0){
#           obs_temp <- c(0,xval)
#           logistic_sample[row_count,] <- obs_temp
#         }else if(sgn1<0){
#           obs_temp <- c(1,xval)
#           logistic_sample[row_count,] <- obs_temp
#         }else if(sgn1 == 0){
#           # delete after dataset generated
#           obs_temp <- c(-1,xval)
#           logistic_sample[row_count,] <- obs_temp
#         }
#         row_count = row_count + 1
#       }
#     }
#   }
#   
#   logistic_sample <- as.data.frame(logistic_sample)
#   colnames(logistic_sample) <- colnames(sample1_no_missing)
#   # delete sgn = 0
#   logistic_sample <- subset(logistic_sample,y!=-1)
#   #head(logistic_sample)
#   #tail(logistic_sample)
  
  #final data list
  final_result <- list(y_logistic=y_logistic,
                       dataset=final_data,
                       #logistic_sample=logistic_sample,
                       method_indicator=method_indicator,
                       gamma=gamma1)
  
  #print(final_data)
  #print(logistic_sample[1:10,])
  #print(dim(logistic_sample))
  return(final_result)
}



