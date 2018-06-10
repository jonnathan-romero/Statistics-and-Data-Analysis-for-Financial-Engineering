library(tseries)
library(fGarch)

dat = read.csv("/Users/jonn/Desktop/Files/Coding/R/dataAnalysisFiles/dataSetsOne/bmwRet.csv",header = TRUE)
names(dat)
attach(dat)
bmw <- BMW.RET 
length(bmw)
#density(bmw)
#?jarque.bera.test
shapiro.test(bmw[1:5000])#if this is really small reject the null that the data is normal
jarque.bera.test(bmw) #if p small reject, and z-squred should be small
muBmw <- mean(bmw)
sd <- sd(bmw)

n <- 20000
mu <- 0
sd <- 1
x <- .95

norm <- rnorm(n, mu, sd)

for (i in 1:length(norm)){
  if (norm[i] < 0) {
    norm[i] <- norm[i]/x
  }else{
    norm[i]<-norm[i]*x
  }
}

hist(norm, breaks = 200)
plot(density(norm))

qqplot(norm,rnorm(n,mu, sd))
qqline(norm,rnorm(n,mu,sd))
