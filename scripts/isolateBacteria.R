# Returns a black and white mask of where bacteria can be found
isolateBacteria <- function(m) {
  
  print("Searching new frame...")
  ptm <- proc.time()
  
#   # normalize
#   m <- m * 0.5/mean(m)
#   # increase contrast
#   m <- (0.4 + m)^3

#   m <- frames[[19]]
  n <- m
  
  # Equalize histogram
  m <- equalize(m) ^ mean(m) 


  
  # Gamma correction
#   m <- equalize(m*n) ^ 0.5

#   # Create glcm masks
  a <- glcm(m, n_grey=15, shift=list(c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.3, statistics=c("dissimilarity"))

  b <- a > 0.5
#   
#   #??????
#   b <- (a[,,1]*3)*m^2 < 0.225
  
#   c <- glcm(b, n_grey=15, shift=list(c(1,1)), 
#             window=c(3,3), min_x=0, max_x=0.5, statistics=c("dissimilarity"))
#   
#   b <- c[,,1] > 0.5
  
  b[artifactMask>0] <- 0
#   b[lineMasks$normal==1] <- 0

  c <- removeBlobs(b, 10)
  kern <- makeBrush(5, shape='disc')
  c <- dilateGreyScale(c, kern)
  kern <- makeBrush(5, shape='disc')
  c <- erodeGreyScale(c, kern)

  c <- removeBlobs(c, 20)
  kern <- makeBrush(9, shape='disc')
  c <- dilateGreyScale(c, kern)
  kern <- makeBrush(9, shape='disc')
  c <- erodeGreyScale(c, kern)

  c <- removeBlobs(c, 50)
  
  kern <- makeBrush(25, shape='disc')
  d <- dilateGreyScale(c, kern)
  kern <- makeBrush(9, shape='disc')
  e <- erodeGreyScale(d, kern)
  
#   e <- removeBlobs(d, 100)
  
  e[m > 0.7] <- 0
  
  e <- removeBlobs(e, 25)
#   e <- dilateGreyScale(e, makeBrush(1, shape="disc"))
  
  print(proc.time() - ptm)

  return(bwlabel(e))
  
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

  print(proc.time() - ptm)
  
  return(bwlabel(h))
  
  }