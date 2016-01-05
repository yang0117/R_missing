#sample generator
#the output would be a list, include gamma value, dataset with missing and dataset for simulation(convert to logistic dataset)
#The name is Gamma_estimate, estimate_indicator, dataset, logistic_sample

#This code is for to remove missing value in the extrem way(remove maximum 20% or minmum 20%), the
#missing rate is based on loss_rate

if(!require("MASS")){
  library(MASS)
}

sample_generator1 <- function(n=100, beta_vector=c(1.5, 2, 3, rep(0,5)), intercept=99,
                              gamma=1, gamma_estimator_switch=T,
                              loss_rate=0.8){
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
  gamma1 <- gamma
  p=length(beta_vector)
  #covariance matrix of covariate
  x_sigma <- matrix(0,p,p)
  for (i in 1:p){
    for (j in 1:p){
      x_sigma[i,j] <- 0.5^abs(i-j)
    }
  }
  #x_sigma[1:10,1:10]
  #generate covariable matrix
  x <- mvrnorm(n,rep(0,p),x_sigma)
  #generate response vector
  #add error
  error_vector <- rnorm(n,0,1)
  x_error <- cbind(x,error_vector)
  y <- x_error %*% c(beta_vector,1) + intercept
  
  #estimate gamma
  
  missing_function <- function(gamma){
    res <- exp(-gamma*abs(y))
    return(res)
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
    
    gamma1 <- uniroot(gamma_eq,c(0,10))$root
    
    #test code
    #   gammatest <- seq(0,1,length=1000)
    #   ytest <- sapply(gammatest, gamma_eq)
    #   plot(gammatest,ytest,type = "l")
    #   ytest  
  }
  missing_binary <- rbinom(n,1,missing_function(gamma1))
  print(missing_binary)
  # loss based on the extrem value
  missing_binary[1:n] <- 1
  print(missing_binary)
  #missing minimum
  missing_index <- order(y)[1:round(n*(1-loss_rate))]
  #missing maximum
  #missing_index <- order(y,decreasing = T)[1:round(n*(1-loss_rate))]
  
  missing_binary[missing_index] <- 0
  print(missing_binary)
  print(y)
  
  
  # make final dataset
  x_name <- character(p)
  for (i in 1:p) x_name[i] <- paste("x",i,sep = "")
  var_name <- c("missing_indicator", "y", x_name)
  final_data <- cbind(missing_binary,y,x)
  colnames(final_data) <- var_name
  final_data <- as.data.frame(final_data)
  
  
  # for logistic dataset
  #delete missing observations
  sample1_no_missing <- subset(final_data,missing_indicator == 1)[,-1]
  
  #covert to logistic regression data
  logistic_sample <- matrix(0, choose(dim(sample1_no_missing)[1],2), dim(sample1_no_missing)[2])
  n_sample1 <- dim(sample1_no_missing)[1]
  row_count <- 1
  for(j in 1:n_sample1){
    for(i in 1:n_sample1){
      if (i<j){
        sgn1 <- -(sample1_no_missing[i,1]-sample1_no_missing[j,1])
        xval <- as.matrix((sample1_no_missing[i,-1]-sample1_no_missing[j,-1])*abs(sample1_no_missing[i,1]-sample1_no_missing[j,1]))
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
  colnames(logistic_sample) <- colnames(sample1_no_missing)
  # delete sgn = 0
  logistic_sample <- subset(logistic_sample,y!=-1)
  #head(logistic_sample)
  #tail(logistic_sample)
  
  #final data list
  final_result <- list(Gamma_estimate=gamma1,
                       estimate_indicator=gamma_estimator_switch,
                       dataset=final_data,
                       logistic_sample=logistic_sample)
  
  #print(final_data)
  #print(logistic_sample[1:10,])
  #print(dim(logistic_sample))
  return(final_result)
}

