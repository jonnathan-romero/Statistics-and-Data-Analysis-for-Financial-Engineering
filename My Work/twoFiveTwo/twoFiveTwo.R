niter = 1e5           
below = rep(0,niter)  
set.seed(2009)
for (i in 1:niter)
{
  r = rnorm(45,mean=.05/253,sd=.23/sqrt(253)) 
  logPrice = log(1e6) + cumsum(r)
  minlogP = min(logPrice) 
  below[i] = as.numeric(minlogP < log(950000))
}
mean(below)
