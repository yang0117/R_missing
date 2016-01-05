#simulation based on cross-validate
if(!require("glmnet")){
  install.packages("glmnet")
  library(glmnet)
}
if(!require("ncvreg")){
  install.packages("ncvreg")
  library(ncvreg)
}

sim1 <- function(logistic_sample,nonzero_num=5){
  #first 5 covariate in nonzero
  #nonzero_num <- 5
 
  #lasso 
  #solve logistic
  lasso_cvfit <- cv.glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial",type.measure = "class")
  #delete intercept
  lasso_nonzero_ind <- coef(lasso_cvfit, s = "lambda.min")@i[-1]
  lasso_total_covariate <- coef(lasso_cvfit, s = "lambda.min")@Dim[1]-1
  tn0en0 <- sum(lasso_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(lasso_nonzero_ind > nonzero_num)
  t0e0 <- lasso_total_covariate - nonzero_num - t0en0
  lasso_res <- c(tn0en0,tn0e0,t0en0,t0e0,lasso_cvfit$lambda.min)
  
  #SCAD
  SCAD_cvfit <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="SCAD")
  #delete intercept
  SCAD_nonzero_ind <- which(coef(SCAD_cvfit, s = "lambda.min")[-1] != 0)
  SCAD_total_covariate <- length(coef(SCAD_cvfit, s = "lambda.min"))-1
  tn0en0 <- sum(SCAD_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(SCAD_nonzero_ind > nonzero_num)
  t0e0 <- SCAD_total_covariate - nonzero_num - t0en0
  SCAD_res <- c(tn0en0,tn0e0,t0en0,t0e0,SCAD_cvfit$lambda.min)
  
  #MCP
  MCP_cvfit <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="MCP")
  #delete intercept
  MCP_nonzero_ind <- which(coef(MCP_cvfit, s = "lambda.min")[-1] != 0)
  MCP_total_covariate <- length(coef(MCP_cvfit, s = "lambda.min"))-1
  tn0en0 <- sum(MCP_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(MCP_nonzero_ind > nonzero_num)
  t0e0 <- MCP_total_covariate - nonzero_num - t0en0
  MCP_res <- c(tn0en0,tn0e0,t0en0,t0e0,MCP_cvfit$lambda.min)
  
  #final result
  res <- list(lasso=lasso_res,SCAD=SCAD_res,MCP=MCP_res)
  return(res)
}

sim1_warning_free <- function(p=50,n=100,
                              gamma=1,gamma_estimator_switch=T,
                              loss_rate=0.8,
                              beta_vector=c(0.5, 0.6, 0.7, 0.8, 0.9, rep(0,45)),
                              nonzero_num=5){
    data1 <- sample_generator1(p=p,n=n,
                               gamma=gamma,gamma_estimator_switch=gamma_estimator_switch,
                               loss_rate=loss_rate,
                               beta_vector=beta_vector)$logistic_sample
  tryCatch({sim1(logistic_sample=data1,nonzero_num = nonzero_num)}, warning=function(w) sim1_warning_free(p=p,n=n,
                                                                                                          gamma=gamma,gamma_estimator_switch=gamma_estimator_switch,
                                                                                                          loss_rate=loss_rate,
                                                                                                          beta_vector=beta_vector,
                                                                                                          nonzero_num=nonzero_num))
}