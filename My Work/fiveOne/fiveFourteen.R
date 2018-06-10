library(fGarch)
library(MASS)
library(glmulti)
dat <- read.csv('/Users/jonn/Desktop/Files/Coding/R/dataAnalysisFiles/dataSetsTwo/Stock_FX_Bond.csv')
names(dat)
attach(dat)

r <- GM_AC[2:length(GM_AC)]/GM_AC[1:(length(GM_AC)-1)]-1
hist(r,breaks=40)

den_r <- density(r)
plot(den_r)

qqnorm(r)

qqplot(r, rnorm(100,0,1))
qqline(r, rnorm(100,0,1))

stdFit_r <- stdFit(r)
fitdistr_r <- fitdistr(r, "t")

sstdFit_r <- sstdFit(r)
