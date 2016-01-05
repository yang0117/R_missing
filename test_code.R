#test code
rm(list=ls())
#test sample generator
source("01.sample_generator.R")

#check stability of Gamma_estimator and missing_rate
n1 = 5
gamma_test <- numeric(n1)
missing_test <- numeric(n1)
for(i in 1:n1){
  res_temp <- sample_generator1()
  gamma_test[i] <- res_temp$Gamma_estimate
  missing_temp <- res_temp$dataset$missing_indicator
  missing_test[i] <- sum(missing_temp)/length(missing_temp)
}

# mean(gamma_test)
# var(gamma_test)
# truehist(gamma_test)
# shapiro.test(gamma_test)
# 
# mean(missing_test)
# var(missing_test)
# truehist(missing_test)
# shapiro.test(missing_test)
