library(png)

n <- length(list.files("frames/", "frame.*")) - 225 # Number of frames
r <- 30.005392    # frame rate
ts <- (0:(n-1))/r # time of each index

# Your initialization here
vec = c()
for (i in 1:n) {
  name <- sprintf("~/Data Science/frames/frame%05d.png", i)
  frame <- readPNG(name)
  rs <- frame[,,1] # Matrix of red values
  gs <- frame[,,2] # Matrix of green values
  bs <- frame[,,3] # Matrix of blue values
  vec = c(vec, sum(rs))
  # Per frame computation here
}
# final computation and visualization here
plot(vec, x=ts, type="l")
