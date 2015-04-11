source("scripts/getCentroids.R")

findSimilarGroups <- function(c1, c2) {
  
  # Extract lengths
  len1 <- dim(c1)[[1]]
  len2 <- dim(c2)[[1]]
  
  b1 <- character(len1*len2)
  b2 <- character(len1*len2)
  dist <- numeric(len1*len2)
  perGrowth <- numeric(len1*len2)
  absGrowth <- numeric(len1*len2)
#   score <- numeric(len1*len2)
  
  for(i in 1:len1) {
    for(j in 1:len2) {
      index <- ((i-1) * len1) + j
      b1[[index]] <- i
      b2[[index]] <- j
      dist[[index]] <- sqrt( (a$x[[i]] - b$x[[j]])^2 + (a$y[[i]] - b$y[[j]])^2 )
      perGrowth[[index]] <- b$size[[j]] / a$size[[i]]
      absGrowth[[index]] <- b$size[[j]] - a$size[[i]]
    }
  }
  
  sorted <- data.frame(b1=b1, b2=b2, dist=dist, perGrowth=perGrowth, absGrowth=absGrowth)
  sorted <- sorted[order(dist),] # sort by distance
  sorted <- sorted[sorted$dist>0,]
  sorted <- sorted[sorted$dist<30,]
  sorted <- sorted[!duplicated(sorted$b1),]
  sorted <- sorted[!duplicated(sorted$b2),]

  return(sorted)

}