
findSimilarGroups <- function(c1, c2) {
  
  # Extract lengths
  len1 <- dim(c1)[[1]]
  len2 <- dim(c2)[[1]]
  
  b1 <- character(len1*len2)
  b2 <- character(len1*len2)
  score <- numeric(len1*len2)
  dist <- numeric(len1*len2)
  
  for(i in 1:len1) {
    for(j in 1:len2) {
      index <- ((i-1) * len1) + j
      b1[[index]] <- i
      b2[[index]] <- j
      dist[[index]] <- sqrt( (c1$x[[i]] - c2$x[[j]])^2 + (c1$y[[i]] - c2$y[[j]])^2 )
      score[[index]] <- dist[[index]] ## / min(c2$size[[j]],c1$size[[i]]) / max(c2$size[[j]],c1$size[[i]])
    }
  }
  
  sorted <- data.frame(b1=as.numeric(b1), b2=as.numeric(b2), dist=dist, score=score)
  sorted <- sorted[score<15,]
  sorted <- sorted[!is.na(sorted$score),]
  sorted <- sorted[order(score),] # sort by score
  sorted <- sorted[!duplicated(sorted$b1),]
  sorted <- sorted[!duplicated(sorted$b2),]

  return(sorted)

}