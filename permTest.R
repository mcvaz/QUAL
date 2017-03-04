### Function that performs permutation test on a data frame

permTest = function(x,v,f,n=9999,testType="bicaudal",plot=T,labx){
  
  # x is a data frame object
  # v is a vector with the names of the variables that will be randomized
  # f is the function to be applied to both the original and permutated data frame (statistic)
  # n is the number of randomizations to be run
  # testType can be either bicaudal (default) or uniUp or uniDown
  
  obs = f(x) # statistic for the observed data
  
  res = numeric() # vector that will keep statistics from each randomization
  
  for(i in 1:n){
    tmp = x # temporary data frame that will contain permutated columns
    for(j in 1:length(v)){
      tmp[,v[j]] = sample(tmp[,v[j]]) # randomization
      }
    res[i] = f(tmp) # records the statistic after each simulation
    }
  
  res = c(res,obs) # adds observed statistic to the n simulated ones

  # plot and probabilities
  hist(res,xlab=labx,main="Permutation test")
  
  # calculates the test probability of finding more extreme values than observed under the null model
  if(testType=="uniUp"){
    pp = sum(res>=obs)/(n+1)
    abline(v=quantile(res,.95),col=2,lty=3)
    }
  if(testType=="uniDown"){
    pp = sum(res<=obs)/(n+1)
    abline(v=quantile(res,.05),col=2,lty=3)
    }
  if(testType=="bicaudal"){
    pp = sum(abs(median(res)-res)>=abs(median(res)-obs))/(n+1)
    abline(v=quantile(res,c(.975,.025)),col=2,lty=3)
    }
  
  points(obs,0,col=4,pch=16) # observed
  legend(x="topright",c(round(obs,2),"","p","=",pp),bty="n",text.col=2) # statistic and p
  }


## Testing the function

# variables
m0 = rnorm(100,100,20)
m1 = m0+rgamma(100,shape=1,scale=10)
Dm = m1-m0
ll = rnorm(100,50,5)  
xx = data.frame(m1,Dm,ll)
cor.test(Dm,m1)
cor.test(m1,ll)

# statistic
ff =  function(x){cor(log((x$Dm)/x$ll),log(x$ll/x$m1))} 
ff(xx)

permTest(x=xx,v="ll",f=ff,labx="Pearson's R")
permTest(x=xx,v=c("ll","Dm"),f=ff,labx="Pearson's R")
permTest(x=xx,v="Dm",f=ff,labx="Pearson's R")
permTest(x=xx,v="m1",f=ff,labx="Pearson's R")
