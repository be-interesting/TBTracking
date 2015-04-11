# Returns a black and white mask of where bacteria can be found
isolateBacteria <- function(m) {
  
  # Create mask of where dark lines are unlikely to contain bacteria
  lineMask <- darkLineMask(m)
  
  # Create glcm masks
  a <- glcm(m, n_grey=18, shift=list(c(0,1),c(1,0),c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.6,
            statistics=c("dissimilarity"))
  
  # Dark out background and apply masks
  b <- (a[,,1]-m)
  b[artifactMask>0] <- 0
  b[lineMask==1] <- 0
  
  # remove pixels over 50% value
  c <- b > 0.5
  
  # Mild dilate/erode to close small gaps
  kern <- makeBrush(3, shape='disc')
  d <- dilateGreyScale(c, kern)
  d <- erodeGreyScale(d, kern)
  
  # Remove small blobs
  e <- removeBlobs(d, 50)
  
  # run dilate, mask, and remove smaller blobs
  f <- dilateGreyScale(e, kern)
  g <- f * (m < 0.45)
  h <- removeBlobs(g, 35)  
  
  return(h)
  
}