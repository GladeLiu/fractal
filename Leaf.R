leaf<-function(n=100000,col="green",cex=2,...){
  x<-c(.5,.5);
  plot(x[1],x[2],xlim=c(-3,3),ylim =c(0,10),type="n",...);
  p <- c(.85,.92,.99,1.00);
  A <- rbind(c(.85,.04),c(-.04,.85),c(.20,-.26),c(.23,.22),
             c(-.15,.28),c(.26,.24),c(0, 0),c(0, .16))
  B <- cbind(c(0,1.6),c(0,1.6),c(0,.44),c(0,0))
  for (i in 1:n){
     ran<-runif(1);
     ind<-rank(c(p,ran),ties.method="min")[5];
     x<-A[(2*ind-1):(2*ind),]%*%x + B[,ind];
    points(x[1],x[2],pch=".",cex=cex,col=col);
  }
} 

png(file ="E:/report/leaf1.png",bg="white");
par(mar=rep(0,4),bty="n");
plot.leaf(cex=1.6,xlab="",ylab="n",xaxt="n",yaxt="n");
dev.off();