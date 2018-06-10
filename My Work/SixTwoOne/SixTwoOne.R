library(readr)
library(bootstrap) 
library(MASS)

options(digits=5)
nboot = 1000

tmle = function(x){as.vector(fitdistr(x,"t")$estimate)}

DowJones30 <- read_csv("/Volumes/USB/Files/Coding/R/dataAnalysisFiles/dataSetsOne/DowJones30.csv")
attach(DowJones30)

GE_Returns=GE[2:length(GE)]/GE[1:length(GE)-1] - 1

results = bootstrap(GE_Returns,nboot,tmle)
apply(results$thetastar[,],1,mean)
apply(results$thetastar[,],1,sd)
fitdistr(GE_Returns,"t")

results_250 = bootstrap(GE_Returns[1:250],nboot,tmle)
apply(results_250$thetastar,1,mean)
apply(results_250$thetastar,1,sd)
fitdistr(GE_Returns[1:250],"t")

par(mfrow=c(1,2))
plot(density(results_250$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(a) n = 250")

plot(density(results$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(b) n = 2528")
