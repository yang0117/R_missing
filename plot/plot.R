#penalty plot

###################
#make penalty plot
##################

#lasso penalty
plasso_fun <- function(x,lambda){
  x <- abs(x)
  res <- lambda*x
  return(res)
}

#SCAD penalty
pSCAD_fun <- function(x,lambda,a){
  x <- abs(x)
  y <- numeric(length(x))
  for(i in 1:length(x)){
    if (x[i] <= lambda){
      y[i] <- lambda*x[i]
    }else if(lambda<x[i] & x[i]<=a*lambda){
     y[i] <- -(x[i]^2-2*a*lambda*x[i]+lambda^2)/(2*(a-1))
    }else{
      y[i] <- (a+1)*lambda^2/2
    }
  }
  return(y)
}

pMCP_fun <- function(x,lambda,a){
  x <- abs(x)
  y <- numeric(length(x))
  for(i in 1:length(x)){
    if (x[i] <= a*lambda){
      y[i] <- lambda*x[i] - x[i]^2/(2*a)
    }else{
      y[i] <- 0.5*a*lambda^2
    }
  }
  return(y)
}
#make plot
x <- seq(0.01,3,by=0.01)
lambda <- 1
a_SCAD <- 2.1
a_MCP <- 2

y_lasso <- plasso_fun(x,lambda)
y_SCAD <- pSCAD_fun(x,lambda,a_SCAD)
y_MCP <- pMCP_fun(x,lambda,a_MCP)

###filename1 = "./plot/penalty_curve.pdf"
pdf("penalty.pdf")
plot(x,y_lasso,type = "l",col=1,lty=1,
     main = "penalty curve",xlab = "t",ylab="p_lambda(t)")
lines(x,y_SCAD,col=2,lty=2)
lines(x,y_MCP,col="blue",lty=3)
legend("topleft",c("LASSO","SCAD","MCP"),inset=.05,
       lty=c(1,2,3), # gives the legend appropriate symbols (lines)
       col=c(1,2,"blue")) # gives the legend lines the correct color and width
dev.off()


###################
#make penalty derivative plot
##################

#lasso penalty
plasso_dev_fun <- function(x,lambda){
  res <- rep(lambda,length(x))
  return(res)
}

# define a positve function
positive_fun <- function(x){
  len = length(x)
  res <- numeric(len)
  for(i in 1:len){
    if (x[i]>0) res[i] <- x[i]
    else res[i] <- 0
  }
  return(res)
}

#SCAD penalty
pSCAD_dev_fun <- function(x,lambda,a){
  x <- abs(x)
  y <- numeric(length(x))
  for(i in 1:length(x)){
    if (x[i] <= lambda){
      y[i] <- lambda
    }else{
      num1 <- (a*lambda-x[i])
      y[i] <- positive_fun(num1)/(a-1)
    }
  }
  return(y)
}

pMCP_dev_fun <- function(x,lambda,a){
  x <- abs(x)
  y <- numeric(length(x))
  for(i in 1:length(x)){
    pos1 <- lambda - x[i]/a
    y[i] <- positive_fun(pos1)
  }
  return(y)
}
#make plot
x <- seq(0.01,3,by=0.01)
lambda_dev <- 1
a_SCAD_dev <- 2.1
a_MCP_dev <- 2

y_lasso_dev <- plasso_dev_fun(x,lambda_dev)
y_SCAD_dev <- pSCAD_dev_fun(x,lambda_dev,a_SCAD_dev)
y_MCP_dev <- pMCP_dev_fun(x,lambda_dev,a_MCP_dev)

###filename1 = "./plot/penalty_derivative_curve.png"
###png(filename=filename1,units="in", width=11, height=8.5, res=300)
pdf("derivative.pdf")
plot(x,y_lasso_dev,type = "l",col=1,lty=1,
     main = "penalty derivative curve",xlab = "t",ylab="p'_lambda(t)",ylim = c(0,3))
lines(x,y_SCAD_dev,col=2,lty=2)
lines(x,y_MCP_dev,col=3,lty=3)
legend("topright",c("LASSO","SCAD","MCP"),inset=.05,
       lty=c(1,2,3), # gives the legend appropriate symbols (lines)
       col=c(1,2,3)) # gives the legend lines the correct color and width
dev.off()


pdf("together.pdf")
par(mfrow=c(1,2))
plot(x,y_lasso,type = "l",col=1,lty=1,
     main = "penalty curve",xlab = "t",ylab="p_lambda(t)")
lines(x,y_SCAD,col=2,lty=2)
lines(x,y_MCP,col="blue",lty=3)
legend("topleft",c("LASSO","SCAD","MCP"),inset=.05,
       lty=c(1,2,3), # gives the legend appropriate symbols (lines)
       col=c(1,2,"blue")) # gives the legend lines the correct color and width
###
plot(x,y_lasso_dev,type = "l",col=1,lty=1,
     main = "penalty derivative curve",xlab = "t",ylab="p'_lambda(t)",ylim = c(0,3))
lines(x,y_SCAD_dev,col=2,lty=2)
lines(x,y_MCP_dev,col="blue",lty=3)
legend("topright",c("LASSO","SCAD","MCP"),inset=.05,
       lty=c(1,2,3), # gives the legend appropriate symbols (lines)
       col=c(1,2,"blue")) # gives the legend lines the correct color and width
dev.off()

###################
#make missing data plot
##################
# require(lattice)
# if(!require("latticeExtra")){
#   install.packages("latticeExtra")
#   require("latticeExtra")
# }
# 
# if(!require("VIM")){
#   install.packages("VIM")
#   install.packages("lme4")
#   require("VIM")
# }
# 
# if(!require("VIMGUI")){
#   install.packages("VIMGUI")
#   require("VIMGUI")
# }
# 
# vmGUImenu()
# aggr()
# matrixplot()

###################
#bar with missing
##################

###filename1 = "./plot/bar_with_missing.png"
###png(filename=filename1,units="in", width=11, height=8.5, res=300)

pdf("datastructure.pdf")
plot(x=0,y=0,xlim=c(0,1.6),ylim=c(0,20),xaxt="n",yaxt="n",axes=F,xlab="",ylab="",type="n")

rect(0.2,10,0.3,19,col=16)
rect(0.5,10,0.6,19,col=16)
rect(0.8,10,0.9,19,col=16)
#rect(1.1,10,1.2,19,col=16)
rect(1.4,10,1.5,19,col=16)

rect(0.2,4,0.3,5,col=16)
rect(0.5,6,0.6,8.5,col=16)
rect(0.8,0.1,0.9,2,col=16)
rect(0.8,5.5,0.9,7,col=16)
#rect(1.1,7,1.2,1,col=16)
rect(1.4,2.5,1.5,3.8,col=16)

text(0.25,20,"Y",col="blue",cex=1,font=3)
text(0.55,20,"X1",col="blue",cex=1,font=3)
text(0.85,20,"X2",col="blue",cex=1,font=3)
text(1.15,20,"...",col="blue",cex=1,font=3)
text(1.45,20,"Xp",col="blue",cex=1,font=3)

text(1.15,14.5,"...",col="blue",cex=1,font=3)
text(1.15,4.5,"...",col="blue",cex=1,font=3)

text(0.15,18.5,"1",col="blue",cex=0.7,font=2)
text(0.15,17,"2",col="blue",cex=0.7,font=2)
text(0.15,14.5,".",col="blue",cex=0.9,font=2)
text(0.15,14,".",col="blue",cex=0.9,font=2)
text(0.15,13.5,".",col="blue",cex=0.9,font=2)
text(0.15,10.5,"n",col="blue",cex=0.7,font=2)

text(0.15,8.5,"n+1",col="blue",cex=0.7,font=2)
text(0.15,5.5,".",col="blue",cex=0.9,font=2)
text(0.15,5,".",col="blue",cex=0.9,font=2)
text(0.15,4.5,".",col="blue",cex=0.9,font=2)
text(0.15,1,"N",col="blue",cex=0.7,font=2)

dev.off()

###################
#bar without missing
##################
###filename1 = "./plot/bar_without_missing.png"
###png(filename=filename1,units="in", width=11, height=8.5, res=300)
pdf("datause.pdf")
plot(x=0,y=0,xlim=c(0,1.6),ylim=c(0,20),xaxt="n",yaxt="n",axes=F,xlab="",ylab="",type="n")

rect(0.2,10,0.3,19,col="red")
rect(0.5,10,0.6,19,col="red")
rect(0.8,10,0.9,19,col="red")
#rect(1.1,10,1.2,19,col=16)
rect(1.4,10,1.5,19,col="red")

rect(0.2,4,0.3,5,col=16)
rect(0.5,6,0.6,8.5,col=16)
rect(0.8,0.1,0.9,2,col=16)
rect(0.8,5.5,0.9,7,col=16)
#rect(1.1,7,1.2,1,col=16)
rect(1.4,2.5,1.5,3.8,col=16)

text(0.25,20,"Y",col="red",cex=1,font=3)
text(0.55,20,"X1",col="red",cex=1,font=3)
text(0.85,20,"X2",col="red",cex=1,font=3)
text(1.15,20,"...",col="red",cex=1,font=3)
text(1.45,20,"Xp",col="red",cex=1,font=3)

text(1.15,14.5,"...",col="red",cex=1,font=3)
text(1.15,4.5,"...",col=16,cex=1,font=3)

text(0.15,18.5,"1",col="red",cex=0.7,font=2)
text(0.15,17,"2",col="red",cex=0.7,font=2)
text(0.15,14.5,".",col="red",cex=0.9,font=2)
text(0.15,14,".",col="red",cex=0.9,font=2)
text(0.15,13.5,".",col="red",cex=0.9,font=2)
text(0.15,10.5,"n",col="red",cex=0.7,font=2)

dev.off()










