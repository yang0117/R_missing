#model part
source("sample_generator.R")
if(!require("glmnet")){
  install.packages("glmnet")
  library(glmnet)
}
if(!require("ncvreg")){
  install.packages("ncvreg")
  library(ncvreg)
}


#lasso


#generate a sample
sample1 <- sample_generator1()
sample1 <- sample1$dataset
#delete missing observations
sample1_no_missing <- subset(sample1,missing_indicator == 1)[,-1]
str(sample1_no_missing)

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

#solve logistic
cvfit <- cv.glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial",type.measure = "class")
plot(cvfit)
coef(cvfit, s = "lambda.min")
class(coef(cvfit, s = "lambda.min"))

names(cvfit)
fit <- glmnet(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial")
print(fit)
plot(fit)

cvfit_SCAD <- cv.ncvreg(as.matrix(logistic_sample[,-1]),logistic_sample[,1],family="binomial",penalty="MCP")
#names(cvfit_SCAD)
cvfit_SCAD$lambda.min
coef(cvfit_SCAD, s = "lambda.min")
plot(cvfit_SCAD)
str(cvfit_SCAD)
fit_SCAD <- ncvreg(as.matrix(logistic_sample[,-1]),factor(logistic_sample[,1],levels=c(0,1)),family="binomial",gamma=10)
plot(fit_SCAD)
print(fit_SCAD)
