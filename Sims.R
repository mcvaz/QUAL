### Use actual distributions of initial biomass, biomass increment, and light interception

## SIM1 - Stadardized major axis regression
source("regr.plot.R")
library(smatr)

sim1 = function(n=100,m0m=100,m0s=20,dsh=1,dsc=10,llm=50,lls=5,graph=TRUE){
  m0 = rnorm(n,m0m,m0s)
  m1 = m0+rgamma(n,shape=dsh,scale=dsc)
  ll = rnorm(n,llm,lls)  
  lue = (m1-m0)/ll
  lie = ll/m1
  modlog = sma(log(lue)~log(lie))
  if(graph){regr.plot(modlog)}
  return(modlog$r2[[1]])
  }

sim1()

res = numeric()
for(i in 1:1000){res[i] = sim1(graph=F)}
hist(res,xlab=as.expression(bquote(R^2)),main="SMA - Simulation")


## SIM2 - Residuals
zz = rnorm(100,10,4) # biomass
xx = 1.5*zz+rnorm(100,0,2.5) # height
yy = 100*zz+rnorm(100,0,100) # crown volume
plot(zz,xx)
plot(zz,yy)
plot(xx,yy)
plot(xx/zz,yy/zz,log="x")
xz = lm(xx~zz)
yz = lm(yy~zz)
plot(xz$residuals,yz$residuals,main="Simulation (residuals)",xlab="Light-size residuals",ylab="Growth-size residuals")
summary(lm(xz$residuals~yz$residuals))
text(5,200,expression(paste(R^2,"= 0")),col=2)


## SIM3 - Permutation test
m0 = rnorm(100,100,20)
m1 = m0+rgamma(100,shape=1,scale=10)
ll = rnorm(100,50,5)  
lue = (m1-m0)/ll
lie = ll/m1
obs = cor(log(lue),log(lie))

res = numeric()
for(i in 1:9999){
  tmpll = sample(ll)
  res[i] = cor(log((m1-m0)/tmpll),log(tmpll/m1))
}
res = c(res,obs)
pp = sum(res<obs)/10000

hist(res,xlab="Pearson's R",main="Permutation test")
abline(v=quantile(res,.05),col=2,lty=3)
arrows(obs,100,obs,0,col=4,length=.1)
legend(x="topright",c("R","=",round(obs,2),"","p","=",pp),bty="n",text.col=2)



