# Combines two images in the order specified
stitchImages <- function(image1, image2, arrange="horizontal") {
  
  d1 <- dim(image1)
  d2 <- dim(image2)
  
  if (arrange=="horizontal") {
    
    if (d1[[2]] != d2[[2]]) {
      stop("Image dimensions don't match")
    }
    
    return(rbind(image1, image2))
    
    
  } else if (arrange=="vertical") {
    
    if (d1[[2]] != d2[[2]]) {
      stop("Image dimensions don't match")
    }
    
    return(cbind(image1, image2))
    
  }
  
}