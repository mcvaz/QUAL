biplot = function(x,y,aa=5,bb=.1){
  
  vv = summary(x)$importance[2,y]
  ss = predict(x)[,y]
  
  plot(ss[,1],ss[,2],type="p",cex=1,pch=16,col="light grey",
       xlab=paste(names(vv)[1]," (",round(vv[1]*100),"%)",sep=""),
       ylab=paste(names(vv)[2]," (",round(vv[2]*100),"%)",sep="")
       )
  abline(v=0,h=0,lty=3)
  
  txtsc = function(x){x+bb*x/abs(x)}
  
  for(i in 1:nrow(x$rotation)){
    tmp = x$rotation[i,y]
    arrows(x0=0,y0=0,x1=tmp[1]*aa,y1=tmp[2]*aa,length=.1)
    text(row.names(pca$rotation)[i],x=txtsc(tmp[1]*aa),
         y=txtsc(tmp[2]*aa),cex=.7)
    }
  
}

