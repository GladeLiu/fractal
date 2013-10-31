Julia<-function(c=1i,res=512,iter=200,xc=0,yc=0,
  xoom=1,color=terrain.colors(30),...){
 #Julia set
 #c is the argument;res is the target resolution;
 #iter is cycle index; (xc,yc) is the center of the graphic;
 #xoom is the magnification times;
  x0<-xc-2/xoom;x1<-xc+2/xoom;
  y0<-yc-2/xoom;y1=yc+2/xoom;
  x<-seq(x0,x1,length=res);
  y<-seq(y0,y1,length=res);
  xx<-t(matrix(rep(x,length(x)),length(x)));
  yy<-matrix(rep(y,length(y)),length(y));
  z<-xx+yy*(1i);
  N<-matrix(0,res,res);
  C<-c*matrix(1,res,res);
   for(k in 1:iter){
     z=z^2+C;
     N[abs(z)>2]=k;
     C[abs(z)>2]=0;
     z[abs(z)>2]=0;
   }
  image(x,y,N,col=color,...);
}

#example
par(mar=rep(0,4));
Julia(1i,512,200,0,0,1,xlab="",ylab="",axes = FALSE);
dev.new();par(mar=rep(0,4));
Julia(1i,512,200,0,0,2000,xlab="",ylab="",axes = FALSE);
dev.new();par(mar=rep(0,4));
Julia(-1.8-0.21i,512,200,0,0,1,xlab="",ylab="",axes = FALSE);