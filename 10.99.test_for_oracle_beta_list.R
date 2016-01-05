#test for 10.01
rm(list=ls())
source("10.01.oracle_beta_list.R")

sample_size<- c(100,500,1000)
sample_size<- c(500,1000)
sample_size<- c(1000)

beta_list <- list(beta_2_0=c(2,0.5,rep(0,2),1.5,rep(0,3)),
                  beta_0.5_2=c(0.5,1.5,2,rep(0,5)),
                  beta_3_0=c(3,1.5,rep(0,2),2,rep(0,3)),
                  beta_1.5_3=c(1.5,2,3,rep(0,5)))
#beta_list <- list(beta_2_0=c(2,0.5,rep(0,2),1.5,rep(0,4)))
beta_list <- list(beta_1.5_3=c(1.5,2,3,rep(0,5)))



#intercept_vec <- c(0,5,10)
intercept_vec <- c(0)

error_independent_vec=c(T,F)


for(i in 1:length(sample_size)){
  for(j in 1:length(beta_list)){
    for(k in 1:length(intercept_vec)){
      for(h in 1:length(error_independent_vec)){
        set.seed(123)
        table_res1 <- oracle_beta_list(n_sim=100,n=sample_size[i],
                                       beta_vector=beta_list[[j]],intercept=intercept_vec[k],
                                       method_indicator="xy",loss_rate=0.625, error_var=1,y_logistic=F,
                                       x_missing_location=1,error_independent=error_independent_vec[h])
        file_location=paste("./oracle_test/data/",names(beta_list)[j],"_n_",sample_size[i],
                            "_intercept_",intercept_vec[k],"_error_independent_",error_independent_vec[h],".Rdata",sep = "")
        save(table_res1,file = file_location)
      }
    }
  }
}







