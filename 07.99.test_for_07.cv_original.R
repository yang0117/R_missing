# test for 07.cv_original.R
rm(list=ls())
source("01.sample_generator.R")
source("07.cv_esimator.R")
#source("07.temp_initial_finder.R")
#####################################################################################################
#test for initial_universial_calculator
#test for penalty indicator
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-99,
                              penalty_indicator="lasso")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-99,
                              penalty_indicator="SCAD")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-99,
                              penalty_indicator="MCP")
#test for lam_indicator
##from cv
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-98,
                              penalty_indicator="lasso")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-98,
                              penalty_indicator="SCAD")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-98,
                              penalty_indicator="MCP")
## user input scalar
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-50,
                              penalty_indicator="lasso")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-50,
                              penalty_indicator="SCAD")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=-50,
                              penalty_indicator="MCP")
## user input vector
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=c(1,2),
                              penalty_indicator="lasso")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=c(1,2),
                              penalty_indicator="SCAD")

initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=5,
                              lam_indicator=c(1,2),
                              penalty_indicator="MCP")
#####################################################################################################
#test for cv_calculator
cv_calculator(lambda=0.1,sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator="wrong_input")
cv_calculator(lambda=0.1,sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator="lasso")
cv_calculator(lambda=0.1,sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator="SCAD")
cv_calculator(lambda=0.1,sample_original=sample_original,logistic_list=logistic_list,k=k,penalty_indicator="MCP")

#####################################################################################################
#test for initial_universial_calculator
#test for k
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=1,
                              lam_indicator=0,
                              penalty_indicator="lasso")
initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=2,
                              lam_indicator=0,
                              penalty_indicator="lasso")

#test for all penalty
p_vec <- c("lasso","SCAD","MCP","wrong")
for (i in p_vec){
  sample1 <- sample_generator1(n=100,beta_vector=c(1.5,2,3,rep(0,5)))
  sample_original <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
  sample_logistic <- sample1$logistic_sample
  test1 <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                                         lam_indicator=-99,
                                         penalty_indicator=i)
  plot(test1$lambda_path,test1$lambda_path_cv)
#   initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
#                                 lam_indicator=-98,
#                                 penalty_indicator=i)
#   initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
#                                 lam_indicator=0,
#                                 penalty_indicator=i)
#   test2 <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
#                                 lam_indicator=c(1,2,3),
#                                 penalty_indicator=i)
#   plot(test2$lambda_path,test2$lambda_path_cv)
}

#####################################################################################################
#test for likelihood function

#verify loglikelihood by logLik
sample1 <- sample_generator1(n=100,beta_vector=c(1.5,2,3,rep(0,5)))
sample_original <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
sample_logistic <- as.matrix(sample1$logistic_sample)
logic_test <- as.matrix(logistic_generator(sample_original))
identical(as.matrix(sample_logistic), logic_test)

beta_est1 = beta_est(sample_logistic=sample_logistic,lambda=0,penalty_indicator="lasso")
#beta_est1
ll <- loglikelihood(sample_original,beta_est1)
ll*choose(dim(sample_original)[1],2)
test_log_fit <- glm(sample_logistic[,1]~sample_logistic[,-1]-1, family=binomial)
logLik(test_log_fit)

#####################################################################################################
#test for data drive plot
rm(list=ls())
source("01.sample_generator.R")
source("07.cv_esimator.R")
k=5
test_time <- 3
penalty_indicator = "lasso"
for(i in 1:test_time){
  sample1 <- sample_generator1(n=100,beta_vector=c(1.5,2,3,rep(0,5)))
  sample_original <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
  sample_logistic <- sample1$logistic_sample
  lam_path1 <- glmnet(as.matrix(sample_logistic[,-1]),factor(sample_logistic[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)$lambda
  lam_path1 <- lam_path1[seq(1,length(lam_path1),by=3)]
  test1 <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                                         lam_indicator=lam_path1,
                                         penalty_indicator=penalty_indicator)
  lambda_cv <- cv.glmnet(as.matrix(sample_logistic[,-1]),factor(sample_logistic[,1],levels=c(0,1)),family="binomial",type.measure = "class")$lambda.min
  
  filename1 = paste("./data_driven_test/",penalty_indicator,"_",i,"_path_plot",".png",sep = "")  
  png(filename=filename1,units="in", width=11, height=8.5, res=300)
  beta_est1 = beta_est(sample_logistic=sample_logistic,lambda=test1$initial_lambda_value,penalty_indicator=penalty_indicator)
  beta_est1 = round(beta_est1,5)
  beta_est1 = paste(beta_est1,collapse=" ")
  plot(test1$lambda_path,test1$lambda_path_cv,main = beta_est1,cex.main=1)
  abline(v=test1$initial_lambda_value,col="red")
  text(x=test1$lambda_path[which.max(test1$lambda_path_cv)],y=min(test1$lambda_path_cv),"our_cv", col = "red")
  abline(v=lambda_cv,col="blue")
  text(x=lambda_cv,y=min(test1$lambda_path_cv),"package_cv", col = "blue")
  dev.off()
}

####################################
#speed test
rm(list=ls())
source("01.sample_generator.R")
source("07.cv_esimator.R")
k=5
test_time <- 3
penalty_indicator = "lasso"
#sample generate
sample1 <- sample_generator1(n=100,beta_vector=c(1.5,2,3,rep(0,5)))
sample_original <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
sample_logistic <- sample1$logistic_sample
lam_path1 <- glmnet(as.matrix(sample_logistic[,-1]),factor(sample_logistic[,1],levels=c(0,1)),family="binomial",intercept = F,standardize = F)$lambda
lam_path1 <- lam_path1[seq(1,length(lam_path1),by=3)]

#dataframe speed
time1 <- proc.time()
speed_res1 <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                                       lam_indicator=lam_path1,
                                       penalty_indicator=penalty_indicator)
time1 <- proc.time() - time1

#matrix speed
sample_original <- as.matrix(sample_original)
#sample_logistic <- as.matrix(sample_logistic)
time2 <- proc.time()
speed_res2 <- initial_universial_calculator(sample_original=sample_original,sample_logistic=sample_logistic,k=k,
                              lam_indicator=lam_path1,
                              penalty_indicator=penalty_indicator)
time2 <- proc.time() - time2
#result
time1
time2
speed_res1[[3]] <- NULL
speed_res2[[3]] <- NULL
identical(speed_res1, speed_res2)
all.equal(speed_res1, speed_res2)
