#01.99.test for 01.sample_generator.R
rm(list=ls())
source("01.sample_generator.R")

#generat test
sample1 <- sample_generator1(n=100,beta_vector=c(2, 0.5, 1, rep(0,5)),intercept=0,
                             loss_rate = 0.625,method_indicator="xy")
sample1$dataset
sum(sample1$dataset$missing_indicator)/100
sd(sample1$dataset$y)
complete_data <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
head(complete_data)
truehist(complete_data[,1])

# data_with_intercept <- sample1$dataset_with_intercept
# head(data_with_intercept)
data_orginal <- sample1$dataset
head(data_orginal)
dim(data_orginal)
sum(data_orginal[,3])
data_logistic <- sample1$logistic_sample
head(data_logistic)
dim(data_logistic)

#generate full dataset
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)

#generate logistics sample
sample_logistic <- sample_generator1(n=200, beta_vector=c(1.5, 2, 3, rep(0,5)), intercept=0,
                    gamma=-999, gamma_estimator_switch=T,
                    loss_rate=0.8,
                    method_indicator="123",
                    error_var=1,
                    y_logistic=T)
sum(sample_logistic$dataset$y)
sum(sample_logistic$dataset$missing_indicator[which(sample_logistic$dataset$y==1)] == 0)
sum(sample_logistic$dataset$missing_indicator)
str(sample_logistic)

#test for "positive" method
sample_positive <- sample_generator1(n=100, beta_vector=c(1.5, 2, 3, rep(0,5)), intercept=0,
                                     gamma=-999, gamma_estimator_switch=T,
                                     loss_rate=0.8,
                                     method_indicator="positive",
                                     error_var=1,
                                     y_logistic=F)
str(sample_positive)
sum(sample_positive$dataset$missing_indicator[which(sample_positive$dataset$y > 0)] == 0)
sum(sample_positive$dataset$missing_indicator[which(sample_positive$dataset$y < 0)] == 1)


#normality test
###################Exp
rm(list=ls())
source("01.sample_generator.R")
##############0.8
#generate sample
sample1 <- sample_generator1(n=250,beta_vector=c(1.5, 2, 3, rep(0,5)),intercept=2,loss_rate = 0.8,method_indicator = "exp")
# data_with_intercept <- sample1$dataset_with_intercept

##full
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)
#histogram
title1 = paste("exp","_full","_08",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_full[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_full[,1],main=title1)
qqline(sample_full[,1],col=2)
dev.off()


##complete
sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
head(sample_complete)
class(sample_complete)
dim(sample_complete)
#histogram
title1 = paste("exp","_complete","_08",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_complete[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_complete[,1],main=title1)
qqline(sample_complete[,1],col=2)
dev.off()

##############0.6
#generate sample
sample1 <- sample_generator1(n=250,beta_vector=c(1.5, 2, 3, rep(0,5)),intercept=2,loss_rate = 0.6)
# data_with_intercept <- sample1$dataset_with_intercept

##full
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)
#histogram
title1 = paste("exp","_full","_06",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_full[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_full[,1],main=title1)
qqline(sample_full[,1],col=2)
dev.off()


##complete
sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
head(sample_complete)
class(sample_complete)
dim(sample_complete)
#histogram
title1 = paste("exp","_complete","_06",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_complete[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_complete[,1],main=title1)
qqline(sample_complete[,1],col=2)
dev.off()


###################Extreme
rm(list=ls())
source("01.sample_generator.R")
##############0.8
#generate sample
sample1 <- sample_generator1(n=200,beta_vector=c(1.5, 2, 3, rep(0,5)),intercept=2,loss_rate = 0.8,method_indicator = "extreme_max")

##full
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)
#histogram
title1 = paste("extrem_min","_full","_08",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_full[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_full[,1],main=title1)
qqline(sample_full[,1],col=2)
dev.off()


##complete
sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
head(sample_complete)
class(sample_complete)
dim(sample_complete)
#histogram
title1 = paste("extrem_min","_complete","_08",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_complete[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_complete[,1],main=title1)
qqline(sample_complete[,1],col=2)
dev.off()

##############0.6
#generate sample
sample1 <- sample_generator1(n=250,beta_vector=c(1.5, 2, 3, rep(0,5)),intercept=2,loss_rate = 0.6,method_indicator = "extreme_min")
# data_with_intercept <- sample1$dataset_with_intercept

##full
sample_full <- as.matrix(sample1$dataset)[,-1]
head(sample_full)
class(sample_full)
dim(sample_full)
#histogram
title1 = paste("extrem_min","_full","_06",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_full[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_full[,1],main=title1)
qqline(sample_full[,1],col=2)
dev.off()


##complete
sample_complete <- as.matrix(subset(sample1$dataset,missing_indicator==1,select = -missing_indicator))
head(sample_complete)
class(sample_complete)
dim(sample_complete)
#histogram
title1 = paste("extrem_min","_complete","_06",sep="")
filename1 = paste("./01.normality_plot/",title1,"_histogram",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
truehist(sample_complete[,1],main=title1)
dev.off()
#qqnorm
filename1 = paste("./01.normality_plot/",title1,"_qqnorm",".png",sep = "")  
png(filename=filename1,units="in", width=11, height=8.5, res=300)
qqnorm(sample_complete[,1],main=title1)
qqline(sample_complete[,1],col=2)
dev.off()



#' test for gamma calculator
rm(list = ls())
source("01.01.gamma_caculator.R")
n_run <- 1000
gamma_vec <- numeric(n_run)
for (i in 1:n_run){
  gamma_vec[i] <- gamma_cal(n=100, beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)), intercept=0,
                            gamma=-999, gamma_estimator_switch=T,
                            loss_rate=0.625,
                            method_indicator="exp",
                            error_var=1,
                            y_logistic=F)$gamma
}

#test for loss rate
loss_rate_vec <- numeric(n_run)
gamma_vec_data <- numeric(n_run)
for(i in 1:n_run){
  data <- gamma_cal(n=100, beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)), intercept=0,
                         gamma=mean(gamma_vec), gamma_estimator_switch=F,
                         loss_rate=0.625,
                         method_indicator="exp",
                         error_var=1,
                         y_logistic=F)
  gamma_vec_data[i] <- data$gamma
  data_temp <- data$dataset
  loss_rate_vec[i] <- sum(data_temp[,1])/dim(data_temp)[1]
}

mean(gamma_vec_data)
mean(loss_rate_vec)

# test for x_missing_location
rm(list=ls())
source("01.sample_generator.R")
source("01.01.gamma_caculator.R")
#should report a an error to show length(beta_vector) should >= x_missing_location
sample1 <- sample_generator1(n=100,beta_vector=c(2, 0.5, rep(0,2), 1,rep(0,3)),intercept=0,
                             loss_rate = 0.625,method_indicator="xy",x_missing_location=10)

#verify gamma will be different when use different x_missing_location=10 and 1
n_run <- 10000
gamma_vec <- numeric(n_run)
for (i in 1:n_run){
  gamma_vec[i] <- gamma_cal(n=100, beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)), intercept=0,
                            gamma=-999, gamma_estimator_switch=T,
                            loss_rate=0.625,
                            method_indicator="xy",
                            error_var=1,
                            y_logistic=F,x_missing_location=1)$gamma
  print(i)
}
mean(gamma_vec)
sd(gamma_vec)


n_run <- 10000
gamma_vec <- numeric(n_run)
for (i in 1:n_run){
  gamma_vec[i] <- gamma_cal(n=100, beta_vector=c(2,0.5,rep(0,2),1,rep(0,3)), intercept=0,
                            gamma=-999, gamma_estimator_switch=T,
                            loss_rate=0.625,
                            method_indicator="xy",
                            error_var=1,
                            y_logistic=F,x_missing_location=8)$gamma
}
mean(gamma_vec)
sd(gamma_vec)


rm(list=ls())
source("01.sample_generator.R")

#generat test
sample1 <- sample_generator1(n=10,beta_vector=c(2, 0.5, 1, rep(0,5)),intercept=0,
                             loss_rate = 1,method_indicator="xy",
                             gamma=-0.1, gamma_estimator_switch=F)
sample1$dataset
sum(sample1$dataset$missing_indicator)/100
complete_data <- subset(sample1$dataset,missing_indicator==1,select = -missing_indicator)
complete_data
head(complete_data)
truehist(complete_data[,1])




