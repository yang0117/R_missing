testf <- function(par1){
  res <- 1/(1+exp(-par1))
}



xtest <- seq(-10,10,length=2000)
xtest <- rnorm(100,0,1)
gamma_test <- seq(-1000,1000,length = 1000)

missing_test <- numeric(length(gamma_test))
for (i in 1:length(gamma_test)){
  par_oneset <- gamma_test[i] * xtest
  ytemp <- sapply(par_oneset, testf)
  missing_test[i] <- mean(ytemp)
}

# ytest <- sapply(xtest, testf)
# mean(ytest)
plot(gamma_test,missing_test)
