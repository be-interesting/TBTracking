# http://www.cs.princeton.edu/~schapire/talks/picasso-minicourse.pdf
# logistic regression ?


findSimilarGroups <- function(c1, c2) {
  
  # Initialize dataframe with each combination of the two indices
  df <- expand.grid(index1=seq(1,dim(c1)[[1]]), index2=seq(1,dim(c2)[[1]]))
  df$id1 <- c1$id[df$index1]
  df$id2 <- c2$id[df$index2]
  
  # Get coordinate arrays for all combinations
  x1 <- c1$x[df$index1]
  y1 <- c1$y[df$index1]
  x2 <- c2$x[df$index2]
  y2 <- c2$y[df$index2]
  
  # Calculate the distances
  df$dist <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
  
  size1 <- c1$size[df$index1]
  size2 <- c2$size[df$index2]
  
  # Calculate the growth
  df$growthPer <- size2 / size1
  df$growthRel <- unlist(lapply(df$growthPer, function(x) if(x > 1) { return(1/x)  } else { return(x) }))
  
  # Calculate "score" of group fit
  df$score <- (0.5^(df$dist/10)) * (df$growthRel^0.1)
  
  # Remove undefinted scores
  df <- df[!is.nan(df$score),]
  
  # Remove unrealistic growth
  df <- df[df$growthPer < 1.5 & df$growthPer > 0.75,]
  
  # Remove unrealistic distance travelled
  df <- df[df$dist < 50,]
  
  # Sort scores
  df <- df[order(-df$score),]
  
  # Remove duplicated ids. This remove weaker connections
  df <- df[!duplicated(df$index1),]
  df <- df[!duplicated(df$index2),]
  

  return(df)

}