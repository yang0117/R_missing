#run cross validation
source("01.sample_generator.R")
source("02.simulation.R")
source("07.cross_validation.R")
source("07.02.cv_plot.R")

#simulation time
n_sim=2
n_col=5
#make lasso SCAD MCP table
#lasso
lasso_result <- matrix(0,n_sim,n_col)
colnames(lasso_result) <- c("tn0en0","tn0e0","t0en0","t0e0","lasso_lambda_est")
lasso_result <- as.data.frame(lasso_result)
lasso_result_package <- as.data.frame(lasso_result)
#SCAD
SCAD_result <- matrix(0,n_sim,n_col)
colnames(SCAD_result) <- c("tn0en0","tn0e0","t0en0","t0e0","SCAD_lambda_est")
SCAD_result <- as.data.frame(SCAD_result)
SCAD_result_package <- as.data.frame(SCAD_result)
#MCP
MCP_result <- matrix(0,n_sim,n_col)
colnames(MCP_result) <- c("tn0en0","tn0e0","t0en0","t0e0","MCP_lambda_est")
MCP_result <- as.data.frame(MCP_result)
MCP_result_package <- as.data.frame(MCP_result)

for(i in 1:n_sim){
  print(i)
  #generate sample
  sample1 <- sample_generator1(n=100,beta_vector=c(1.5,2,3,rep(0,5)))
  sample_original <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
  sample_logistics <- sample1$logistic_sample
  
  #find best lambda from cross validation based on the original method
  result_cv <- cv_original(sample_original=sample_original,sample_logistics=sample_logistics,k=5,nonzero_num=3,
                           lam_ini_lasso=-99,lam_ini_SCAD=-99,lam_ini_MCP=-99)
  
  
  #plot lambda path vs cv value
  filename1 = paste("./pic_cv", "/",i,"_lambda_path",".png",sep = "")  
  png(filename=filename1,units="in", width=11, height=8.5, res=300)
  par(mfrow=c(3,1))
  plot(result_cv$lasso_path,main="lasso")
  plot(result_cv$SCAD_path,main="SCAD")
  plot(result_cv$MCP_path,main="MCP")
  dev.off()
  
  #find best lambda from cv in the package
  result_cv_package <- sim1(sample_logistics,nonzero_num=3)
  
  print(result_cv)
  print(result_cv_package)
  
  #cross validation from original data
  lasso_result[i,]<-result_cv$lasso
  SCAD_result[i,]<-result_cv$SCAD
  MCP_result[i,]<-result_cv$MCP
  
  #cross validation from package
  lasso_result_package[i,]<-result_cv_package$lasso
  SCAD_result_package[i,]<-result_cv_package$SCAD
  MCP_result_package[i,]<-result_cv_package$MCP
  
  #plot
  lambda_plot(beta_vector = c(1.5, 2, 3, rep(0,5)), 
              sample_size = 100,
              pic_loc = "/pic_cv",
              run_count_ind = i,logistic_sample=sample_logistics,
              lam_lasso=result_cv$lasso[5],
              lam_SCAD=result_cv$SCAD[5],
              lam_MCP=result_cv$MCP[5])
}















