getCentroids <- function(m) {
  
  x <- numeric(max(m))
  y <- numeric(max(m))
  size <- numeric(max(m))
  id <- character(max(m))
  index <- numeric(max(m))
  
  generateSeq <- function(x, y,z) {
    return(paste0("x", x, "y", y,"z",z))
  }
  
  for (i in 1:max(m)) {
    m1 <- m == i
    ind <- which(m1, arr.ind=T)
    x[[i]] <- mean(ind[,1])
    y[[i]] <- mean(ind[,2])
    size[[i]] <- sum(m1)
    id[[i]] <- generateSeq(round(mean(ind[,1])),round(mean(ind[,2])),sum(m1))
    index[[i]] <- i
  }
  
  df <- data.frame(x=x,y=y,size=size,id=id,index=index)
  
  df <- df[size>0,]

  return(df)
  
}


