lambda_plot <- function(beta_vector = c(1.5, 2, 3, rep(0,5)), 
                        sample_size = 100,
                        pic_loc = "/pic",
                        run_count_ind = 0,logistic_sample,
                        lam_lasso,
                        lam_SCAD,
                        lam_MCP){
  #   beta_vector <- c(1.5, 2, 3, rep(0,5))
  #   sample_size <- 100
  #   pic_loc = "/pic"
  # run_count_ind: used when save a filename
  
  p = length(beta_vector)
  
  #number of non-zero parameter 
  num_non_zero <- p - sum(beta_vector==0)
  #######model
  #####lasso
  fit_lasso <- glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial")
  fit_lasso_cv <- cv.glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial",type.measure = "class")
  
  
  #find out smallest lambda cause estimation of noe-zero parameter is non-zero
  lambda_lasso <- fit_lasso$lambda
  #length(lambda_lasso)
  # log(lambda_lasso[1])
  # log(lambda_lasso[100])
  # 
  # log(lambda_lasso[1],base = 10)
  # log(lambda_lasso[100], base = 10)
  
  lambda_fit <- -1
  
  for (i in length(lambda_lasso):1){
    if (length(coef(fit_lasso,lambda_lasso[i])@i[-1])==num_non_zero){
      if (sum(coef(fit_lasso,lambda_lasso[i])@i[-1]==1:num_non_zero)){
        lambda_fit <- lambda_lasso[i]
        break
      }
    }
  }
  
  lambda_cv <- fit_lasso_cv$lambda.min
  
  
  
  
  title1 = paste("beta =",paste(beta_vector[which(beta_vector!=0)],collapse = " "),"p =",p,"n =",sample_size,collapse = "; ")
  filename1 = paste(".", pic_loc,"/",run_count_ind,"_",title1,".png",sep = "")  
  
  png(filename=filename1,units="in", width=11, height=8.5, res=300)
  par(mfrow=c(3,1))
  title1 = paste("lasso beta =",paste(beta_vector[which(beta_vector!=0)],collapse = " "),"p =",p,"n =",sample_size,collapse = "; ")
  plot(fit_lasso,label=T,xvar="lambda",main=title1)
  abline(h=0,col = "black")
  abline(v=log(lambda_fit),col="red")
  text(x=log(lambda_fit),y=0, "smallest", col = "red")
  abline(v=log(lambda_cv),col="blue")
  text(x=log(lambda_cv),y=0, "cv", col = "blue")
  abline(v=log(lam_lasso),col="black")
  text(x=log(lam_lasso),y=0, "cv_new", col = "black")
  
  #####SCAD
  fit_SCAD <- ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="SCAD")
  fit_SCAD_cv <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="SCAD")
  
  
  #find out smallest lambda cause estimation of noe-zero parameter is non-zero
  lambda_SCAD <- fit_SCAD$lambda
  #length(lambda_SCAD)
  # log(lambda_SCAD[1])
  # log(lambda_SCAD[100])
  # 
  # log(lambda_SCAD[1],base = 10)
  # log(lambda_SCAD[100], base = 10)
  
  lambda_fit <- -1
  
  for (i in length(lambda_SCAD):1){
    coef_vector <- coef(fit_SCAD,lambda_SCAD[i])
    non_zero_ind <- which(coef_vector!=0)
    if (length(non_zero_ind)==num_non_zero){
      if (sum(non_zero_ind==2:(num_non_zero+1))){
        lambda_fit <- lambda_SCAD[i]
        break
      }
    }
  }
  
  lambda_cv <- fit_SCAD_cv$lambda.min
  
  
  
  title1 = paste("SCAD beta =",paste(beta_vector[which(beta_vector!=0)],collapse = " "),"p =",p,"n =",sample_size,collapse = "; ")
  
  plot(fit_SCAD,log.l = T,main=title1)
  abline(v=log(lambda_fit),col="red")
  text(x=log(lambda_fit),y=0, "smallest", col = "red")
  abline(v=log(lambda_cv),col="blue")
  text(x=log(lambda_cv),y=0, "cv", col = "blue")
  abline(v=log(lam_SCAD),col="black")
  text(x=log(lam_SCAD),y=0, "cv_new", col = "black")
  
  #####MCP
  fit_MCP <- ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="MCP")
  fit_MCP_cv <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="MCP")
  
  
  #find out smallest lambda cause estimation of noe-zero parameter is non-zero
  lambda_MCP <- fit_MCP$lambda
  #length(lambda_MCP)
  # log(lambda_MCP[1])
  # log(lambda_MCP[100])
  # 
  # log(lambda_MCP[1],base = 10)
  # log(lambda_MCP[100], base = 10)
  
  lambda_fit <- -1
  
  for (i in length(lambda_MCP):1){
    coef_vector <- coef(fit_MCP,lambda_MCP[i])
    non_zero_ind <- which(coef_vector!=0)
    if (length(non_zero_ind)==num_non_zero){
      if (sum(non_zero_ind==2:(num_non_zero+1))){
        lambda_fit <- lambda_MCP[i]
        break
      }
    }
  }
  
  lambda_cv <- fit_MCP_cv$lambda.min
  
  title1 = paste("MCP beta =",paste(beta_vector[which(beta_vector!=0)],collapse = " "),"p =",p,"n =",sample_size,collapse = "; ")
  
  filename1 = paste(".", pic_loc,"/",title1,".png",sep = "")
  plot(fit_MCP,log.l = T,main=title1)
  abline(v=log(lambda_fit),col="red")
  text(x=log(lambda_fit),y=0, "smallest", col = "red")
  abline(v=log(lambda_cv),col="blue")
  text(x=log(lambda_cv),y=0, "cv", col = "blue")
  abline(v=log(lam_MCP),col="black")
  text(x=log(lam_MCP),y=0, "cv_new", col = "black")
  dev.off()
}
