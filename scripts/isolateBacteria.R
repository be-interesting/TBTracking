
isolateBacteria <- function(m) {
  
  print("Searching new frame...")
  ptm <- proc.time()
  
  # m <- frames[[4]]
  
  # Calculate homogeneity
  a <- glcm(m, n_grey=25, window=c(3,3), statistics=c("homogeneity"))
  
  # This essentially finds edges
  b <- 1 - (1-a[,,1])^3
  
  # Multiplying the edges by the original image darkens only the edges of the bacteria.
  # This reduces the chance of dark regions in the background being captured.
  c <- (b * m)
  
  if (norm(m) < 670) {
    c <- c < 0.275
  } else {
    c <- c < 0.25
  }


  
  # Apply artifact mask
  c[artifactMask>0] <- 0
  
  c <- dilateGreyScale(c, makeBrush(3, shape='disc'))
  c <- erodeGreyScale(c, makeBrush(3, shape='disc'))
  
  c <- removeBlobs(c, 5)
  
  # Mild dilate/erode to close small gaps
  d <- dilateGreyScale(c, makeBrush(7, shape='disc'))
  d <- erodeGreyScale(d, makeBrush(7, shape='disc'))
  
  # Remove very small blobs
  e <- removeBlobs(d, 10)
  
  # Larger dilate/erode. This lets groups capture small shards of white that should
  # count as the same group.
  f <- dilateGreyScale(e, makeBrush(11, shape='disc'))
  f <- erodeGreyScale(f, makeBrush(11, shape='disc'))
  
  # Now remove less small blobs.
  g <- removeBlobs(f, 25)
  
  # Large dilate to garuntee all of the area around the bacteria is selected.
  h <- dilateGreyScale(g, makeBrush(11, shape='disc'))
  
  # Equalize m, which flattens the value histogram. This greatly increases the
  # contrast around the edges. Then select the lighter areas from that region and
  
  if (norm(m) < 670) {
    h[(equalize(m)) > 0.6] <- 0
  } else {
    h[(equalize(m)) > 0.4] <- 0
  }
  
  
  
  # Remove noise
  h <- removeBlobs(h, 20)
  
  # Dilate to bring groups together
  h <- dilateGreyScale(h, makeBrush(3))
  
  # Now label sections
  h <- bwlabel(h)
  
  # Since sections are already labeled it's safe to remove a lot of the excess
  # White so we get a more accurate reading
  if (norm(m) < 670) {
    h[m > 0.45] <- 0
  } else {
    h[m > 0.4] <- 0
  }
  
 
  
  h[artifactMask>0] <- 0
  
  print(proc.time() - ptm)
  
  return(h)
  
}


