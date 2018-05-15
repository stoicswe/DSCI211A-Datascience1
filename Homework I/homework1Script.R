getwd()
setwd("/Users/nathanbunch/Data Science/")
potholeData <- read.csv("potholes.csv")
potholeDataClean <- potholeData[c("lat", "long")]
plot(potholeDataClean$lat, potholeDataClean$long)
hist(potholeDataClean$lat, 10)