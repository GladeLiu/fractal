Mandelbrot<-function(res=512,iter=128,xc=0,yc=0,
  xoom=1,color=terrain.colors(30),...){
 #Mandelbrot set
 #res is the target resolution;
 #iter is cycle index; (xc,yc) is the center of the graphic;
 #xoom is the magnification times;
  x0<-xc-2/xoom;x1<-xc+2/xoom;
  y0<-yc-2/xoom;y1<-yc+2/xoom;
  x<-seq(x0,x1,length=res);
  y<-seq(y0,y1,length=res);
  xx<-t(matrix(rep(x,length(x)),length(x)));
  yy<-matrix(rep(y,length(y)),length(y));
  z<-xx+yy*(1i);C<-z;
  N<-matrix(0,res,res); 
  for(k in 1:iter){     
    z=z^2+C;        
    N[abs(z)>4]=k;  
    z[abs(z)>4]=0;
    C[abs(z)>4]=0;
  }
 image(x,y,N,col=color,...); 
}

#example
par(mar=rep(0,4));
Mandelbrot(512,100,0,0,1,xlab="",ylab="",axes = FALSE);
dev.new();par(mar=rep(0,4));
Mandelbrot(512,128,-1.478,0,300,xlab="",ylab="",axes = FALSE);