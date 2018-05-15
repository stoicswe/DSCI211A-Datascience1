library(png)

name <- sprintf("~/Data Science/appleSm2.png", 1)
frame <- readPNG(name)
rs <- frame[,,1] # Matrix of red values
gs <- frame[,,2] # Matrix of green values
bs <- frame[,,3] # Matrix of blue values

redVal = sum(rs)
grnVal = sum(gs)
bluVal = sum(bs)

percentOfAreaCovered = (redVal / (redVal + bluVal))
percentOfAreaCovered
paperArea = 8.5*11
paperArea
areaOfApple = paperArea*percentOfAreaCovered
areaOfApple
