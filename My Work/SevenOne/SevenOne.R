library(readr)
DowJones30 <- read_csv("/Volumes/USB/Files/Github/Statistics-and-Data-Analysis-for-Financial-Engineering/Book/dataSetsOne/DowJones30.csv")
names(DowJones30)
options(digits=3)

returns = function(x){
  return(x[2:length(x)]/x[1:length(x)-1]-1)
}

DowJones30_Returns <- as.data.frame(sapply(DowJones30[2:31],returns))

cov(DowJones30_Returns)
cor(DowJones30_Returns)
plot(DowJones30_Returns[1:30])


