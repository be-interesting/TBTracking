getCentroids <- function(m) {
  
  m <- bwlabel(m)
  
  x <- c()
  y <- c()
  z <- c()
  
  for (i in 1:max(m)) {
    m1 <- m == i
    ind <- which(m1, arr.ind=T)
    x <- c(x, mean(ind[,1]))
    y <- c(y, mean(ind[,2]))
    z <- c(z, sum(m1))
  }
  
  return(data.frame(x=x,y=y,size=z))
  
}


