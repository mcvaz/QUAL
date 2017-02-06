### Use actual distributions of initial biomass, biomass increment, and light interception



n = 30
cols = grey(.5,.5)
m0=rnorm(n,100,20)
hist(m0)
m1=m0+rgamma(n,shape=1,scale=10)
#m1=m0+rnorm(n,10,3)
hist(m1)
hist(m1-m0)
plot(m1~m0)
abline(a=0,b=1,lty=2,col=2)
ll=rnorm(n,50,5)
hist(ll)
lue=(m1-m0)/ll
hist(lue)
hist(log(lue))
lie=ll/m1
hist(lie)
hist(log(lie))
#plot(lue~lie,col=cols,pch=16)
#mod = lm(lue~lie)
#summary(mod)
#abline(mod,col=2)
plot(log(lue)~log(lie),col=cols,pch=16)
modlog = lm(log(lue)~log(lie))
summary(modlog)
abline(modlog,col=2)



## SIM2
zz = rnorm(100,10,4) # biomass
xx = 1.5*zz+rnorm(100,0,2.5) # height
yy = 100*zz+rnorm(100,0,100) # crown volume
plot(zz,xx)
plot(zz,yy)
plot(xx,yy)
plot(xx/zz,yy/zz,log="x")
xz = lm(xx~zz)
yz = lm(yy~zz)
plot(xz$residuals,yz$residuals)
summary(lm(xz$residuals~yz$residuals))
