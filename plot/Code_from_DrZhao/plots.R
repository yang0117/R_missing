
rm(list=ls(all=TRUE))

> getwd()
[1] "E:/Wisc_backup/NMARJiang/MonotoneNFD/cd4ar1"

pdf("yx.pdf")
plot(x=0,y=0,xlim=c(0,2),ylim=c(0,12),xaxt="n",yaxt="n",axes=F,xlab="",ylab="",type="n")
rect(0.75,4,1,10)
rect(1.25,1,1.5,10)
text(0.375,10.5,"R",col="blue",cex=1.5)
text(0.875,10.5,"Y",col="blue",cex=1.5)
text(1.375,10.5,"X",col="blue",cex=1.5)
text(0.375,9.75,"1",cex=1.5)
text(0.375,8.75,"1",cex=1.5)
text(0.375,5.50,"1",cex=1.5)
text(0.375,4.375,"1",cex=1.5)
text(0.375,3.625,"0",cex=1.5)
text(0.375,1.25,"0",cex=1.5)

points(0.375,6.75,pch=20)
points(0.375,7.25,pch=20)
points(0.375,7.75,pch=20)

points(0.375,1.75,pch=20)
points(0.375,2.25,pch=20)
points(0.375,2.75,pch=20)
dev.off()

pdf("yuz.pdf")
plot(x=0,y=0,xlim=c(0,2.5),ylim=c(0,12),xaxt="n",yaxt="n",axes=F,xlab="",ylab="",type="n")
rect(0.75,4,1,10)
rect(1.25,1,1.5,10)
rect(1.75,1,2,10)
text(0.375,10.5,"R",col="blue",cex=1.5)
text(0.875,10.5,"Y",col="blue",cex=1.5)
text(1.375,10.5,"U",col="blue",cex=1.5)
text(1.875,10.5,"Z",col="blue",cex=1.5)
text(0.375,9.75,"1",cex=1.5)
text(0.375,8.75,"1",cex=1.5)
text(0.375,5.50,"1",cex=1.5)
text(0.375,4.375,"1",cex=1.5)
text(0.375,3.625,"0",cex=1.5)
text(0.375,1.25,"0",cex=1.5)

points(0.375,6.75,pch=20)
points(0.375,7.25,pch=20)
points(0.375,7.75,pch=20)

points(0.375,1.75,pch=20)
points(0.375,2.25,pch=20)
points(0.375,2.75,pch=20)
dev.off()





cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)
g_range <- range(0, cars, trucks)
plot(cars, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c("Mon","Tue","Wed","Thu","Fri"))
axis(2, las=1, at=4*0:g_range[2])
box()
lines(trucks, type="o", pch=22, lty=2, col="red")
title(main="Autos", col.main="red", font.main=4)
title(xlab="Days", col.lab=rgb(0,0.5,0))
title(ylab="Total", col.lab=rgb(0,0.5,0))
legend(1, g_range[2], c("cars","trucks"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2);


naive <- c(2.979157, 2.835932, 2.827198, 2.595663, 2.565613)
mar <-   c(2.979157, 2.845875, 2.722237, 2.562880, 2.401697)
non <-   c(2.979157, 2.851190, 2.638144, 2.634376, 2.299671)
range.1 <- range(naive, mar, non)
plot(naive, type="b", col="black", ylim=c(2.0,3.5), axes=FALSE, ann=FALSE)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.0,3.5,0.1), lab=seq(2.0,3.5,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red")
lines(non, type="b", pch=23, lty=3, col="blue")
title(main="Treatment 4: Triple Therapy", col.main="red", font.main=3)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.5, c("naive","mar","nonignorable"), cex=0.8, col=c("black","red","blue"), text.col=c("black","red","blue"), pch=21:23, lty=1:3);


# treatment 2
naive <- c(2.934099, 2.948798, 2.937148, 2.606258, 2.671962)
mar <-   c(2.934099, 2.959299, 2.739777, 2.511117, 2.434904)
non <-   c(2.934099, 2.999577, 2.748451, 2.357726, 2.455849)

# treatment 3
naive <- c(2.906508, 3.106939, 2.983998, 2.791312, 2.796732)
mar <-   c(2.906508, 3.076327, 2.906891, 2.779490, 2.533517)
non <-   c(2.906508, 3.051009, 2.899071, 2.832628, 2.547533)

# treatment 4
naive <- c(2.835652, 3.153987, 3.239560, 2.898972, 2.959384)
non <-   c(2.835652, 3.090439, 3.176539, 2.905543, 2.759226)
mar <-   c(2.835652, 3.262974, 2.758628, 2.811377, 2.713925)


# simulation
naive <- c(2.906508, 3.051002, 2.899063, 2.832622, 2.547526)
mar <-  c(2.906508, 3.273824, 3.362942, 3.072349, 2.409354)
non <-  c(2.906508, 3.043692, 2.895145, 2.762739, 2.504642)



pdf("trt2.pdf")
naive <- c(2.934099, 2.948798, 2.937148, 2.606258, 2.671962)
non <-   c(2.934099, 2.959299, 2.739777, 2.511117, 2.434904)
mar <-   c(2.934099, 2.999577, 2.748451, 2.357726, 2.455849)
range.1 <- range(naive, mar, non)
plot(naive, type="b", col="black", ylim=c(2.3,3.4), axes=FALSE, ann=FALSE, lwd=1.5)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.3,3.4,0.1), lab=seq(2.3,3.4,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red", lwd=1.5)
lines(non, type="b", pch=23, lty=3, col="blue", lwd=2)
title(main="ZDV plus ddI", col.main="red", font.main=4)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.55, c("naive","simple","proposed"), cex=1, col=c("black","red","blue"), text.col=c("black","red","blue"), pch=21:23, lty=1:3);

dev.off()

pdf("trt2start.pdf")
naive <- c(2.934099, 2.948798, 2.937148, 2.606258, 2.671962)
mar <-   c(2.934099, 2.999577, 2.748451, 2.357726, 2.455849)
range.1 <- range(naive, mar)
plot(naive, type="b", col="black", ylim=c(2.3,3.4), axes=FALSE, ann=FALSE, lwd=2)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.3,3.4,0.1), lab=seq(2.3,3.4,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red", lwd=2)
title(main="ZDV plus ddI", col.main="red", font.main=4)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.5, c("naive","simple"), cex=1, col=c("black","red"), text.col=c("black","red"), pch=21:22, lty=1:2);
dev.off()


pdf("trt4.pdf")
naive <- c(2.835652, 3.153987, 3.239560, 2.898972, 2.959384)
non <-   c(2.835652, 3.090439, 3.176539, 2.905543, 2.759226)
mar <-   c(2.835652, 3.262974, 2.758628, 2.811377, 2.713925)
range.1 <- range(naive, mar, non)
plot(naive, type="b", col="black", ylim=c(2.3,3.4), axes=FALSE, ann=FALSE, lwd=1.5)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.3,3.4,0.1), lab=seq(2.3,3.4,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red", lwd=1.5)
lines(non, type="b", pch=23, lty=3, col="blue", lwd=2)
title(main="Triple Therapy", col.main="red", font.main=4)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.55, c("naive","simple","proposed"), cex=1, col=c("black","red","blue"), text.col=c("black","red","blue"), pch=21:23, lty=1:3);

dev.off()

pdf("trt4start.pdf")
naive <- c(2.835652, 3.153987, 3.239560, 2.898972, 2.959384)
mar <-   c(2.835652, 3.262974, 2.758628, 2.811377, 2.713925)
range.1 <- range(naive, mar)
plot(naive, type="b", col="black", ylim=c(2.3,3.4), axes=FALSE, ann=FALSE, lwd=2)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.3,3.4,0.1), lab=seq(2.3,3.4,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red", lwd=2)
title(main="Triple Therapy", col.main="red", font.main=4)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.5, c("naive","simple"), cex=1, col=c("black","red"), text.col=c("black","red"), pch=21:22, lty=1:2);
dev.off()



pdf("simu.pdf")
naive <- c(2.906508, 3.051002, 2.899063, 2.832622, 2.547526)
mar <-  c(2.906508, 3.273824, 3.362942, 3.072349, 2.409354)
non <-  c(2.906508, 3.043692, 2.895145, 2.762739, 2.504642)
range.1 <- range(naive, mar, non)
plot(naive, type="b", col="black", ylim=c(2.3,3.4), axes=FALSE, ann=FALSE, lwd=2)
axis(1, at=1:5, lab=c(0, 8, 16, 24, 32))
axis(2, at=seq(2.3,3.4,0.1), lab=seq(2.3,3.4,0.1),las=1)
box()
lines(mar, type="b", pch=22, lty=2, col="red", lwd=2)
lines(non, type="b", pch=23, lty=3, col="blue", lwd=2)
title(main="Longitudinal Means for Simulated Data", col.main="red", font.main=4)
title(xlab="Weeks", col.lab=rgb(0,0.5,0))
title(ylab="log(CD4+1)", col.lab=rgb(0,0.5,0))
legend(1, 2.55, c("truth","MAR","proposed"), cex=1, col=c("black","red","blue"), text.col=c("black","red","blue"), pch=21:23, lty=1:3);

dev.off()



