library(png)
library(quantmod)
getwd()
n <- length(list.files("frames/", "frame.*")) - 225 # Number of frames
r <- 30.005392    # frame rate
ts <- (0:(n-1))/r # time of each index

# Your initialization here
vec = c()
for (i in 1:n) {
  name <- sprintf("frames/frame%05d.png", i)
  frame <- readPNG(name)
  rs <- frame[,,1] # Matrix of red values
  gs <- frame[,,2] # Matrix of green values
  bs <- frame[,,3] # Matrix of blue values
  
  vec = c(vec, sum(rs))
}

# final computation and visualization here

tsLess = c(ts < 10)
tslessNew= c(ts(tsLess))
tslessNew
plot(vec, type="l")
plot(y=vec, x= ts, type="l")
rpoints =c(seq(from=1, to=n, by=8))
newVec = c()
for (i in seq(from=1, to=n, by=8)) { newVec= c(newVec, vec[i] ) }
plot(newVec, x=ts[rpoints], type="l")

heartRate = length(findPeaks(newVec, thresh=0))
heartBpm = heartRate*6
heartBpm
