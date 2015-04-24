getCentroids <- function(m) {
  
  x <- numeric(max(m))
  y <- numeric(max(m))
  size <- numeric(max(m))
  id <- character(max(m))
  
  generateSeq <- function() {
    x <- floor(runif(10,1,26.9))
    return(paste(LETTERS[x], collapse = ""))
  }
  
  for (i in 1:max(m)) {
    m1 <- m == i
    ind <- which(m1, arr.ind=T)
    x[[i]] <- mean(ind[,1])
    y[[i]] <- mean(ind[,2])
    size[[i]] <- sum(m1)
    id[[i]] <- generateSeq()
    
  }
  
  df <- data.frame(x=x,y=y,size=size,id=id)
  
  df <- df[size>0,]

  return(df)
  
}


