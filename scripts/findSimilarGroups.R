# http://www.cs.princeton.edu/~schapire/talks/picasso-minicourse.pdf
# logistic regression ?


findSimilarGroups <- function(c1, c2) {
  
  # Extract lengths
  len1 <- dim(c1)[[1]]
  len2 <- dim(c2)[[1]]
  
  b1 <- character(len1*len2)        # store index
  b2 <- character(len1*len2)        # store index
  score <- numeric(len1*len2)       # measurement of group fit
  dist <- numeric(len1*len2)        # distance
  id1 <- character(len1*len2)
  id2 <- character(len1*len2)
  growthAbs <- numeric(len1*len2)   # growth in pixels, -inf -> inf
  growthRel <- numeric(len1*len2)   # percent difference in size, 0 -> 1
  growthPer <- numeric(len1*len2)   # percent growth, 0 -> inf
  
  i <- 0
  for(ii in 1:len1) {
    g1 <- c1[ii,]
    for(jj in 1:len2) {
      
      g2 <- c2[jj,]
      
      i <- i + 1
 
      b1[[i]] <- g1$index
      b2[[i]] <- g2$index
      
      dist[[i]] <- sqrt( (g1$x - g2$x)^2 + (g1$y - g2$y)^2 )
      
      growthAbs[[i]] <- g2$size - g1$size     
      growthRel[[i]] <- min(g1$size, g2$size)/max(g1$size, g2$size)
      growthPer[[i]] <- g2$size / g1$size
      
      # We want the id from the FIRST frame
      id1[[i]] <- as.character(g1$id)
      id2[[i]] <- as.character(g2$id)
      
      score[[i]] <- 0.5^(dist[[i]]/10) * growthRel[[i]]^0.1
      
    }
  }
  
  sorted <- data.frame(b1=as.numeric(b1), b2=as.numeric(b2), dist=dist, 
                       score=score, id1=id1, id2=id2,
                       growthPer=growthPer, growthAbs=growthAbs)
  
  sorted <- sorted[!is.na(sorted$b1),]
  sorted <- sorted[order(-sorted$score),] # sort by score
  sorted <- sorted[!duplicated(sorted$b1),]
  sorted <- sorted[!duplicated(sorted$b2),]
  sorted <- sorted[sorted$growthPer < 1.4 & sorted$growthPer > 0.9,]
  sorted <- sorted[sorted$dist < 30,]
  
  ret <- data.frame(cbind(as.character(sorted$id1), as.character(sorted$id2), sorted$b1, sorted$b2))
  colnames(ret) <- c("id1", "id2", "b1", "b2")

  return(ret)

}