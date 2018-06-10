yOne = rnorm(10000,3.1,4)/5.3
yTwo = rnorm(10000,3,1)/5
y=c(yOne, yTwo)
x=density.default(x=y, type = "gaussian")
x
plot(x)

x=density.default(x=rnorm(10000,0,1), type = "gaussian")
x
plot(x)

w=rnorm(10000,0,1)
plot(quantile(w))

stocks <- read.csv("~/Desktop/Files/Coding/R/dataAnalysisFiles/dataSetsTwo/Stock_FX_Bond.csv", header=TRUE)
attach(stocks)
names(stocks)

IBMReturns <- (IBM_AC[2:length(IBM_AC)]/IBM_AC[1:length(IBM_AC)-1])-1
plot(density(IBMReturns))
plot(IBM_AC,type="l")
plot(IBMReturns,type="l")
plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1))
for (i in 1:1000){
  x=pnorm(q=rnorm(1000,0,1))
  q=quantile(x)
  points(q,c(0,0.25,.5,.75,1),xdata=TRUE)
}

plot(1, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1))
for (i in 1:100){
  x=pnorm(q=rnorm(1001,0,1),0,1)
  points(x=sort(x,decreasing = FALSE),y=seq(0,1,1/1000),xdata=TRUE)
}
qqnorm(y=x)#give is probabilitys and it gives you the quintile
qqline(y=x)

xOne=rnorm(1001,0,1)
xTwo=rnorm(1001,3,.4)
qqplot(y=x,x = x)
