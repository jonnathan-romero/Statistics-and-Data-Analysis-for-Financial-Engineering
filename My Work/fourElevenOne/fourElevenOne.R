dat = read.csv("/Users/jonn/Desktop/Files/Coding/R/dataAnalysisFiles/dataSetsTwo/Stock_FX_Bond.csv")
names(dat)
attach(dat)

plot(x=Date[1:length(Date)-1],y=diff(log(MSFT_AC)),type="o")

Log_MSFT_AC=diff(log(MSFT_AC))
hist(Log_MSFT_AC)
density(Log_MSFT_AC)     
plot(density(Log_MSFT_AC))

qqnorm(y=x) #looks normal
qqline(y=x)

qqplot(y=x,x=rnorm(n=length(Date)-1,0,1))#has fatter tails
qqline(y=x)
