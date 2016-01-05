#######################################old#####################################################
#######################################old#####################################################
#######################################old#####################################################
#######################################old#####################################################
#######################################old#####################################################
cv_original <- function(sample_original,sample_logistics,k=5, nonzero_num = 3,
                        lam_indicator_lasso=-99,lam_indicator_SCAD=-99,lam_indicator_MCP=-99){
  #   #parameter
  #   k=5
  #   lam_ini_lasso = -99
  #   lam_ini_SCAD = -99
  #   lam_ini_MCP = -99
  
  # find the initial value of lambda for nlm()
  # lam_ini = -99 means get the initial value from the path generate by the package(which minimize the function of lambda)
  # lam_ini = 0 means use 0 as initial value
  # lam_ini = -98 means get the initial value from cross validation method provided by the package
  
  # check k is in the correct range
  if(k<2 | k>dim(sample_original)[1]) stop("k should be greater than 1 and less than the rows in the original data(after deleting missing)")
  
  
  #####################
  #01. generate the initial value for nlm() method
  #####################
  
  #assign initial values
  lam_ini_lasso <- -999
  lam_ini_SCAD <- -999
  lam_ini_MCP <- -999
  
  #check initial values
  if (lam_ini_lasso == -999) stop("initial value for lasso in nlm() is wrong")
  if (lam_ini_SCAD == -999) stop("initial value for SCAD in nlm() is wrong")
  if (lam_ini_MCP == -999) stop("initial value for MCP in nlm() is wrong")
  
  #01.get the initial value for nlm()
  
  
  ############################################
  #get the path of lambda
  #lasso
  if (lam_indicator_lasso == -99){
    lam_ini_lasso <- glmnet(as.matrix(sample_logistics[,-1]),factor(sample_logistics[,1],levels=c(0,1)),family="binomial")$lambda
  }
  #SCAD
  if (lam_indicator_SCAD == -99){
    lam_ini_SCAD <- ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="SCAD")$lambda
  }
  #MCP
  if (lam_indicator_MCP == -99){
    lam_ini_MCP <- ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="MCP")$lambda
  }
  
  
  
  
  
  
  
  
  result1 <- cv_value(sample_original=sample_original,k=k,
                      lambda_list_lasso = lam_ini_lasso,
                      lambda_list_SCAD = lam_ini_SCAD,
                      lambda_list_MCP = lam_ini_MCP)
  
  ########################################
  #make result table
  #lasso
  #delete intercept
  fit_lasso <- glmnet(as.matrix(sample_logistics[,-1]),factor(sample_logistics[,1],levels=c(0,1)),family="binomial",lambda = min_lasso[1])
  lasso_nonzero_ind <- coef(fit_lasso, s = min_lasso[1])@i[-1]
  lasso_total_covariate <- coef(fit_lasso, s = min_lasso[1])@Dim[1]-1
  tn0en0 <- sum(lasso_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(lasso_nonzero_ind > nonzero_num)
  t0e0 <- lasso_total_covariate - nonzero_num - t0en0
  lasso_res <- c(tn0en0,tn0e0,t0en0,t0e0,min_lasso[1])
  
  #SCAD
  fit_SCAD <-  ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="SCAD",lambda =  as.numeric(min_SCAD[1]))
  #delete intercept
  SCAD_nonzero_ind <- which(coef(fit_SCAD, s = min_SCAD[1])[-1] != 0)
  SCAD_total_covariate <- length(coef(fit_SCAD, s = min_SCAD[1]))-1
  tn0en0 <- sum(SCAD_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(SCAD_nonzero_ind > nonzero_num)
  t0e0 <- SCAD_total_covariate - nonzero_num - t0en0
  SCAD_res <- c(tn0en0,tn0e0,t0en0,t0e0,min_SCAD[1])
  
  #MCP
  fit_MCP <- ncvreg(as.matrix(sample_logistics[,-1]),sample_logistics[,1],family="binomial",penalty="MCP",lambda =  as.numeric(min_MCP[1]))
  #delete intercept
  MCP_nonzero_ind <- which(coef(fit_MCP, s = min_MCP[1])[-1] != 0)
  MCP_total_covariate <- length(coef(fit_MCP, s = min_MCP[1]))-1
  tn0en0 <- sum(MCP_nonzero_ind <= nonzero_num)
  tn0e0 <- nonzero_num - tn0en0
  t0en0 <- sum(MCP_nonzero_ind > nonzero_num)
  t0e0 <- MCP_total_covariate - nonzero_num - t0en0
  MCP_res <- c(tn0en0,tn0e0,t0en0,t0e0,min_MCP[1])
  
  #final result
  res <- list(lasso=lasso_res,lasso_path=result1$lasso,
              SCAD=SCAD_res,SCAD_path=result1$SCAD,
              MCP=MCP_res,MCP_path=result1$MCP)
  return(res)
}