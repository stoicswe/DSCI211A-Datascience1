getwd()
setwd("/Users/nathanbunch/Data Science")
dir()
walmartSourceData <- read.csv("features.csv")
temperatureAverage <- c(1:45)
for (x in 1:45){
  temperatureAverage[x] <- sum(walmartSourceData$Temperature[walmartSourceData$Store == x])
  temperatureAverage[x] <- temperatureAverage[x] / length(walmartSourceData$Temperature[walmartSourceData$Store == x])
}
store <- c(1:45)
plot(store, temperatureAverage)
