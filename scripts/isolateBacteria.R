
isolateBacteria <- function(m) {
  
  # m <- frames[[4]]
  
  # Calculate homogeneity
  a <- glcm(m, window=c(5,5), statistics=c("homogeneity"))
  
  # This essentially finds edges
  b <- 1 - (1-a[,,1])^3
  
  c <- (b * m) < 0.25
  
  c[artifactMask>0] <- 0
  
  # Mild dilate/erode to close small gaps
  
  d <- dilateGreyScale(c, makeBrush(7, shape='disc'))
  d <- erodeGreyScale(d, makeBrush(7, shape='disc'))
  
  # Remove small blobs
  e <- removeBlobs(d, 10)
  
  f <- dilateGreyScale(e, makeBrush(11, shape='disc'))
  f <- erodeGreyScale(f, makeBrush(11, shape='disc'))
  
  g <- removeBlobs(f, 25)
  
  h <- dilateGreyScale(g, makeBrush(15, shape='disc'))
  
  h[(equalize(m)^0.5) > 0.4] <- 0
  
  h <- removeBlobs(h, 25)
  
  h <- dilateGreyScale(h, makeBrush(3))
  
  return(bwlabel(h))
  
}


