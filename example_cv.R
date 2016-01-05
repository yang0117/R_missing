rm(list=ls())
if(!require("MASS")){
  library(MASS)
}

if(!require("glmnet")){
  library(glmnet)
}

#function to generate sample
sample_generator1 <- function(n=100, beta_vector=c(1.5, 2, 3, rep(0,5)), intercept=0){
  p=length(beta_vector)
  x_sigma <- matrix(0,p,p)
  for (i in 1:p){
    for (j in 1:p){
      x_sigma[i,j] <- 0.5^abs(i-j)
    }
  }
  x <- mvrnorm(n,rep(0,p),x_sigma)

    #generate normal distribution sample and missing indicator
    #generate response vector
    #add error
    error_vector <- rnorm(n,0,1)
    x_error <- cbind(x,error_vector)
    y <- x_error %*% c(beta_vector,1) + intercept
    gamma1 <- gamma
  
  # make final dataset
  x_name <- character(p)
  for (i in 1:p) x_name[i] <- paste("x",i,sep = "")
  var_name <- c("y", x_name)
  final_data <- cbind(y,x)
  colnames(final_data) <- var_name
  final_data <- as.matrix(final_data)
  
  return(final_data)
}


################
#
###############
#generate sample
set.seed(123)
sample_gaussian <- sample_generator1(n=100,beta_vector=c(3, 1.5, 2, rep(0,5)),intercept=0)
k=5
n_sim <- 10
lambda_vec <- numeric(n_sim)
for(i in 1:n_sim){
  cat("\nThis is",i,"simulation",sep = " ")
  fit_cv_lasso <- cv.glmnet(sample_gaussian[,-1],sample_gaussian[,1],family="gaussian",nfolds=k,standardize = T)
  cat("\nLambda path is\n")
  print(fit_cv_lasso$lambda)
  fit_cv_lambda <- fit_cv_lasso$lambda.min
  lambda_vec[i] <- fit_cv_lasso$lambda.min
  coef1 <- coef(fit_cv_lasso,s = fit_cv_lambda)
  cat("\nEstimation from cross-validation is\n")
  print(coef1)
}
print(lambda_vec)


