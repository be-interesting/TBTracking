# Returns a black and white mask of where bacteria can be found
isolateBacteriaoff <- function(m) {
  
  print("Searching new frame...")
  ptm <- proc.time()
  
  # normalize
  m <- m * 0.5/mean(m)
  # increase contrast
  m <- (0.4 + m)^3
  
  
  
  # Create mask of where dark lines are unlikely to contain bacteria
  #   lineMasks <- darkLineMask(m)
  # Darken areas not in the line mask
  #   m[lineMasks$inverse] <- m[lineMasks$inverse] / 3
  
  # Create glcm masks
  a <- glcm(m, n_grey=15, shift=list(c(1,1)), 
            window=c(3,3), min_x=0, max_x=max(m), statistics=c("homogeneity"))
  
  #??????
  b <- m * (m + a[,,1])
  
  c <- glcm(b, n_grey=15, shift=list(c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.5, statistics=c("dissimilarity"))
  
  b <- c[,,1] > 0.5
  
  b[artifactMask>0] <- 0
  #   b[lineMasks$normal==1] <- 0
  
  kern <- makeBrush(11, shape='disc')
  d <- dilateGreyScale(b, kern)
  d <- erodeGreyScale(d, kern)
  
  e <- removeBlobs(d, 50)
  
  e[m > 0.75] <- 0
  
  e <- removeBlobs(e, 25)
  
  print(proc.time() - ptm)
  
  return(bwlabel(e))
  
}




isolateBacteria <- function(m) {
  
  # m <- frames[[4]]
  
  a <- glcm(m, window=c(5,5), statistics=c("homogeneity"))
  
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


