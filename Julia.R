Julia<-function(c,res,iter,xc,yc,xoom){
#library(pracma);
x0<-xc-2/xoom;
x1<-xc+2/xoom;
y0<-yc-2/xoom;y1=yc+2/xoom;
x<-seq(x0,x1,length=res);
y<-seq(y0,y1,length=res);
xx<-t(matrix(rep(x,length(x)),length(x)));
yy<-matrix(rep(y,length(y)),length(y));
z<-xx+yy*(1i);
N<-zeros(res,res);
C<-c*ones(res,res);
for(k in 1:iter){
   z=z^2+C;
   N[abs(z)>2]=k;
   C[abs(z)>2]=0;
   z[abs(z)>2]=0;
}
image(x,y,N,col=rainbow(max(N)));
}

Julia(1i,512,200,0,0,1);
Julia(1i,512,200,0,0,2000);
Julia(-0.8-0.21i,512,200,0,0,1);