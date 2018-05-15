library(igraph)
library(grDevices)
setwd("~/Data Science/") #only if linux OS

favColorData <- read.csv("Favorite_Color_Data.csv")
favColorMatrix <- data.frame(favColorData$What.is.your.major., favColorData$What.is.your.favorite.color.)
majors <- unique(favColorMatrix$favColorData.What.is.your.major.)
colors <- unique(favColorMatrix$favColorData.What.is.your.favorite.color.)
favColors <- matrix(0, nrow = length(majors), ncol = length(colors))
rownames(favColors) = majors
colnames(favColors) = colors
for (i in 1:nrow(favColorMatrix)){
  majr <- which(favColorMatrix[i,1] == majors)
  colr <- which(favColorMatrix[i,2] == colors)
  favColors[majr,colr] = favColors[majr,colr] + 1
}
edges <- matrix(0, nrow = length(majors), ncol = length(majors))
colorMatrix <- matrix(0, nrow = length(majors), ncol = length(majors))
rownames(edges) = majors
colnames(edges) = majors
rownames(colorMatrix) = majors
colnames(colorMatrix) = majors
favColumns <- ncol(favColors)
favRows <- nrow(favColors)
favRowsK <- favRows-1
for (i in 1:favColumns){
  for (k in 1:favRows){
    for (j in k:favRowsK){
      checkOne <- favColors[k, i]
      checkTwo <- favColors[j, i]
      checkOne <- checkOne > 0
      checkTwo <- checkTwo > 0
      if (checkOne && checkTwo){
        row <- rownames(favColors)[k]
        col <- colnames(favColors)[i]
        row2 <- rownames(favColors)[j]
        edges[row,row2] = 1
        colorMatrix[row,row2] = col
      }
    }
  }
}
adjacencyGraph <- graph.adjacency(edges, weighted = TRUE)
getColors <- get.edges(adjacencyGraph, E(adjacencyGraph))
getColorsColumns <- ncol(getColors)
getColorsRows <- nrow(getColors)
finalColors <- c()
for (i in 1:getColorsRows){
  finalColors <- c(finalColors, adjustcolor(colorMatrix[getColors[i,1],getColors[i,2]], alpha.f = .5))
}
E(adjacencyGraph)$color = finalColors
E(adjacencyGraph)$weight <- edge.betweenness(adjacencyGraph)
V(adjacencyGraph)$color <- "gray"
l <- layout.graphopt(adjacencyGraph, charge = 100000000000000000000, mass = 10000000, spring.length = 100, spring.constant = 1, max.sa.movement = 5)
plot(simplify(adjacencyGraph, remove.multiple = F), layout = l, edge.arrow.size=.01, vertex.label.color="black", vertex.label.dist=.2, vertex.size = 4)
