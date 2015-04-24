getCentroids <- function(m) {
  
#   m <- bwlabel(m)
  
  x <- numeric(max(m))
  y <- numeric(max(m))
  z <- numeric(max(m))
  id <- character(max(m))
  index <- numeric(max(m))
  
  generateSeq <- function() {
    x <- floor(runif(10,1,26.9))
    return(paste(LETTERS[x], collapse = ""))
  }
  
  for (i in 1:max(m)) {
    m1 <- m == i
    ind <- which(m1, arr.ind=T)
    x[[i]] <- mean(ind[,1])
    y[[i]] <- mean(ind[,2])
    z[[i]] <- sum(m1)
    id[[i]] <- generateSeq()
    index[[i]] <- i
  }
  
  return(data.frame(x=x,y=y,size=z,id=id,index=index))
  
}


