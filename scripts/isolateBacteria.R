# Returns a black and white mask of where bacteria can be found
isolateBacteria <- function(m) {
  
  # normalize
  m <- m * 0.5/mean(m)
  # increase contrast
  m <- (0.4 + m)^3
  
  
  
  # Create mask of where dark lines are unlikely to contain bacteria
  lineMasks <- darkLineMask(m)
  # Darken areas not in the line mask
  m[lineMasks$inverse] <- m[lineMasks$inverse] / 3
  
  # Create glcm masks
  a <- glcm(m, n_grey=15, shift=list(c(1,1)), 
            window=c(3,3), min_x=0, max_x=max(m), statistics=c("homogeneity"))
  
  #??????
  b <- m * (m + a[,,1])
  
  c <- glcm(b, n_grey=15, shift=list(c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.5, statistics=c("dissimilarity"))
  
  b <- c[,,1] > 0.5
  
  b[artifactMask>0] <- 0
  b[lineMasks$normal==1] <- 0
  
  kern <- makeBrush(9, shape='disc')
  d <- dilateGreyScale(b, kern)
  kern <- makeBrush(9, shape='disc')
  d <- erodeGreyScale(d, kern)
  
  e <- removeBlobs(d, 50)
  
  e[m > 0.7] <- 0
  
  e <- removeBlobs(e, 25)

  return(e)
  
}




isolateBacteriaOff <- function(m) {  
  
  # normalize
  m <- m * 0.5/mean(m)
  # increase contrast
  m <- (0.4 + m)^2.5

  # Create mask of where dark lines are unlikely to contain bacteria
  lineMasks <- darkLineMask(m)
  
  # Darken areas not in the line mask
  m[lineMasks$inverse] <- m[lineMasks$inverse] / 3
  
  # Create glcm masks
  a <- glcm(m, n_grey=20, shift=list(c(0,1),c(1,0),c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.5)
#             statistics=c("homogeneity"))
  
  b <- (m * a[,,1]) < 0.3
  

  
  b[artifactMask>0] <- 0
  b[lineMasks$normal==1] <- 0
  
  
  
  # Mild dilate/erode to close small gaps
  kern <- makeBrush(7, shape='disc')
  d <- dilateGreyScale(b, kern)
  kern <- makeBrush(7, shape='disc')
  d <- erodeGreyScale(d, kern)
  
  # Remove small blobs
  e <- removeBlobs(d, 25)
  
  kern <- makeBrush(5, shape='disc')
  # run dilate, mask, and remove smaller blobs
  f <- dilateGreyScale(e, kern)
  f <- erodeGreyScale(f, kern)
  g <- f * (m < 0.55)
  h <- removeBlobs(g, 15)  
  
  return(bwlabel(h))
  
  }