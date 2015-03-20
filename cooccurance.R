# Calculate texture metrics based on cooccurance matrices
# https://courses.cs.washington.edu/courses/cse576/book/ch7.pdf

# calculate normalized cooccurance matrix
coocc <- function(m, breaks=10) {
  # generates a normalized cooccurance matrix
  m <- round(m*breaks)
  c <- matrix(0, breaks, breaks)
  for (x1 in 1:breaks) {
    for (y1 in 1:breaks) {
      for (y2 in 1:dim(m)[[2]] - 1) {
        for (x2 in 1:dim(m)[[1]] - 1) {
          v0 <- m[x2,y2]
          v1 <- m[x2+1,y2+1]
          c[v0,v1] <- c[v0,v1] + 1
        }
      }
    }
  }
  return(c/sum(c))
}

# Energy
energy <- function(m) {
  return(sum(m^2))
}

# Entropy
entropy <- function(m) {
  sm <- 0
  for(i in 1:dim(m)[[1]]) {
    for(j in 1:dim(m)[[2]]) {
      n <- try(m[i,j] * log2(m[i,j]))
      if(!is.nan(n)) sm <- sm + n
    }
  }
  return(sm * -1)
}

# Contrast
contrast <- function(m) {
  sm <- 0
  for(i in 1:dim(m)[[1]]) {
    for(j in 1:dim(m)[[2]]) {
      sm <- sm + (((i-j)^2) * m[i,j])
    }
  }
  return(sm)
}

# Homogeneity
homogeneity <- function(m) {
  sm = 0
  for(i in 1:dim(m)[[1]]) {
    for(j in 1:dim(m)[[2]]) {
      s <- m[i,j] / (1 + abs(i - j))
      sm <- sm + s
    }
  }
  return(sm)
}

# Correlation 
correlation <- function(m) {
  sm = 0
  for(i in 1:dim(m)[[1]]) {
    for(j in 1:dim(m)[[2]]) {

    }
  }
  return(sm)
}