#sample generator
#the output would be a list, include gamma value and dataset

if(!require("MASS")){
  library(MASS)
}

sample_generator1 <- function(p=50,n=100,
                              gamma=1,gamma_estimator_switch=T,
                              loss_rate=0.8,
                              beta_vector=c(0.5, 0.6, 0.7, 0.8, 0.9, rep(0,45))){
  # #number of covariate
  # p <- 50
  # #number of obs
  # n <- 100
  # #other parameters
  # 
  # loss_rate <- 0.8
  # beta_vector <- c(0.5, 0.6, 0.7, 0.8, 0.9, rep(0,45))
  # gamma_estimator_switch <- T
  # 
  gamma1 <- gamma
  
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
  y <- x_error %*% c(beta_vector,1)
  
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
  
  # make final dataset
  x_name <- character(p)
  for (i in 1:p) x_name[i] <- paste("x",i,sep = "")
  var_name <- c("missing_indicator", "y", x_name)
  final_data <- cbind(missing_binary,y,x)
  colnames(final_data) <- var_name
  final_data <- as.data.frame(final_data)
  
  #final data list
  final_result <- list(Gamma_estimate=gamma1,estimate_indicator=gamma_estimator_switch,dataset=final_data)
  return(final_result)
}
