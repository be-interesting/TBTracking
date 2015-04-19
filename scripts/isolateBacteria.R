# Returns a black and white mask of where bacteria can be found
isolateBacteriaoff <- function(m) {
  
  ### TEST save this
  m <- (0.3 + m)^2
  
  # Create mask of where dark lines are unlikely to contain bacteria
  lineMasks <- darkLineMask(m)
  
  # Darken areas not in the line mask
#   m[lineMasks$inverse] <- m[lineMasks$inverse] / 1.5
  
  # Create glcm masks
  a <- glcm(m, n_grey=20, shift=list(c(0,1),c(1,0),c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.45)
  
  b <- a[,,5]
  
#   # Dark out background and apply masks
#   b <- (a[,,5]-m) * (m < 0.4)
#   
#   # remove pixels over 50% value
#   c <- a[,,5] > 0.6 & m < 0.4
  
  b[artifactMask>0] <- 0
  b[lineMasks$normal==1] <- 0

  c <- b > 0.85
  
  # Mild dilate/erode to close small gaps
  kern <- makeBrush(5, shape='disc')
  d <- dilateGreyScale(c, kern)
  d <- erodeGreyScale(d, kern)
  
  # Remove small blobs
  e <- removeBlobs(d, 50)

  e[a[,,3]>0.75] <- 0
  
  kern <- makeBrush(5, shape='disc')
  # run dilate, mask, and remove smaller blobs
  f <- dilateGreyScale(e, kern)
  f <- erodeGreyScale(f, kern)
  g <- f * (m < 0.45)
  h <- removeBlobs(g, 15)  
  
  return(bwlabel(h))
  
}




isolateBacteria <- function(m) {  
  
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
            window=c(3,3), min_x=0, max_x=0.5, statistics=c("homogeneity"))
  
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