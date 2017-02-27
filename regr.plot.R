regr.plot = function(mod) {
  r2 = round(mod$r2[[1]],2)
  pp = mod$pval
  if(pp<0.001){sign="***"}else{if(pp<0.01){sign="**"}else{if(pp<0.05){sign="*"}else{sign=""}}}
  cols = grey(.5,.5)
  plot(x=mod$data[,2],y=mod$data[,1],xlab="log(LIE)",ylab="log(LUE)",col=cols,pch=16,main="SMA - Simulation")
  legend(x="topright",c(as.expression(bquote(R^2)),"=",r2,sign),text.col=2,bty="n")
  abline(mod,col=2)
  }