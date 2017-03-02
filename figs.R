pdf("diagPlot1.pdf",width=20)
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by height")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot2.pdf",width=20)
tmp = tmp[order(tmp$ext),]
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by light interception")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot3.pdf",width=20)
tmp = tmp[order(tmp$above),]
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by incident light")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot4.pdf",width=20)
tmp = tmp[order(tmp$below),]
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by transmitted light")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot5.pdf",width=20)
tmp = tmp[order(tmp$above),]
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by incident light",log="y")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot6.pdf",width=20)
tmp = tmp[order(tmp$below),]
plot(tmp$cmax,ylab="Light",type="n",ylim=rr,main="Ordered by incident light",log="y")
#abline(v=1:nrow(tmp),col=grey(0.1,alpha=.5),lty=3)
for(i in 1:nrow(tmp)){
  points(x=rep(i,5),y=tmp[i,c("il1","il2","il3","il4","il5")],col=rainbow(1,alpha=.5),pch=16)
  points(x=rep(i,5),y=tmp[i,c("tl1","tl2","tl3","tl4","tl5")],col=rainbow(1,start=0.5,alpha=.5),pch=16)
}
dev.off()

pdf("diagPlot7.pdf",width=20)
tmp2 = tmp2[order(tmp2$cmax),]
plot(tmp2$cmax,ylab="Height (m)",type="n",ylim=rr2,main="Ordered by height")
for(i in 1:nrow(tmp2)){
  segments(i,tmp2[i,"cmin"],i,tmp2[i,"cmax"],col=grey(0.5,alpha=0.5))
  points(i,tmp2[i,"cmean"],pch=16,cex=.5,col=grey(0.5,alpha=0.5))
}
dev.off()

pdf("diagPlot8.pdf",width=20)
tmp2 = tmp2[order(tmp2$cmin),]
plot(tmp2$cmax,ylab="Height (m)",type="n",ylim=rr2,main="Ordered by bottom height")
for(i in 1:nrow(tmp2)){
  segments(i,tmp2[i,"cmin"],i,tmp2[i,"cmax"],col=grey(0.5,alpha=0.5))
  points(i,tmp2[i,"cmean"],pch=16,cex=.5,col=grey(0.5,alpha=0.5))
}
dev.off()

pdf("diagPlot9.pdf",width=20)
tmp2 = tmp2[order(tmp2$cmean),]
plot(tmp2$cmax,ylab="Height (m)",type="n",ylim=rr2,main="Ordered by mean height")
for(i in 1:nrow(tmp2)){
  segments(i,tmp2[i,"cmin"],i,tmp2[i,"cmax"],col=grey(0.5,alpha=0.5))
  points(i,tmp2[i,"cmean"],pch=16,cex=.5,col=grey(0.5,alpha=0.5))
}
dev.off()



