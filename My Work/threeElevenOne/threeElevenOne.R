bondvalue = function(c,T,r,par){
  #        c = coupon payment (semiannual)
  #        T = time to maturity (in years)
  #        r = vector of yields to maturity (semiannual rates)
  #        par = par value
  bv = c/r + (par - c/r) * (1+r)^(-2*T)
  bv
}

price = 1200 
C=40 
T=30 
par = 1000

r = seq(.02,.05,length=300)
value = bondvalue(C,T,r,par)
yield2M = spline(value,r,xout=price)

plot(r,value,xlab="yield to maturity",ylab="price of bond",type="l",main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M)

uniroot(function(r) r^2-.5, c(0.7,0.8)) #this finds the root between these vales

#********************************************************************************
mkMaturity = read.csv("/Users/jonn/Desktop/Files/Coding/R/threeElevenOne/mk.maturity.csv",header=TRUE)
attach(mkMaturity)
View(mkMaturity)

mkZero2 = read.csv("/Users/jonn/Desktop/Files/Coding/R/threeElevenOne/mk.zero2.csv",header=TRUE)
attach(mkZero2)
View(mkZero2)

plot(mkMaturity[,1],mkZero2[5,2:56],type="l",xlab="maturity",ylab="yield")
lines(mkMaturity[,1],mkZero2[6,2:56],lty=2,type="l")
lines(mkMaturity[,1],mkZero2[7,2:56],lty=3,type="l")
lines(mkMaturity[,1],mkZero2[8,2:56],lty=4,type="l")
legend("bottomright",c("1985-12-01", "1986-01-01","1986-02-01", "1986-03-01"),lty=1:4)

