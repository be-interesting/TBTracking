# Creates white pixel mask which removes artifacts from dataset.
# Artifacts include grided black circles and large squares which
# designate slide number.

# Assume the background will be passed in
createArtifactMask <- function(bg) {
  
  # Extract pixel data for speed's sake
  bg.data <- bg@.Data
  output <- bg@.Data
  
  # Get dimensions
  dim.x <- dim(bg.data)[[1]]
  dim.y <- dim(bg.data)[[2]]
  
  # Set mask to matrix of dim xy
  outputMask <- matrix(1, dim.x, dim.y)
  
  # TEST
  # Isolate the y ranges to search to speed things up
  highs <- c(121, 215, 303, 393, 487, 575, 667, 755, 849, 935, 1025, 1112, 1205, 1295, 1385, 1475, 1565)
  # Add 20 indices in between each element of highs to search
  yIndices <- unlist(lapply(highs, function(x) seq(x, x+20)))
  

  # Loop through the y indices
  for (y in yIndices) {
    
    # For each y value, search through all of the x values and find similar areas
    for (x in seq(1, dim.x-31, 4)) {
      
      # Get subset of original
      subset <- bg.data[x:(x+30),y:(y+30)]
      
      # Arbitrary threshold
      thr <- 0.04
      
      dx.beg <- mean(abs(diff(subset[15,0:10])))
      dx.mid <- mean(abs(diff(subset[15,12:18])))
      dx.end <- mean(abs(diff(subset[15,20:30])))
      dy.beg <- mean(abs(diff(subset[15,0:10])))
      dy.mid <- mean(abs(diff(subset[15,12:18])))
      dy.end <- mean(abs(diff(subset[15,20:30])))
      
      if(dx.mid < thr & dy.mid < thr # low change in mids
         & dx.beg - dx.end < thr & dy.beg - dy.end < thr # beg similar to end
         & dx.beg > thr & dx.end > thr # high change in beg/end
         & dy.beg > thr & dx.end > thr # high change in beg/end
         ) {
        
        # mask subset with white circle
        subset <- output[x:(x+30),y:(y+30)] * createFillCircle(31, 31, 10)
        outputMask[x:(x+30),y:(y+30)] <- outputMask[x:(x+30),y:(y+30)] * createFillCircle(31, 31, 10)
        subset[subset==0] <- 1
        
#         output[x:(x+30),y:(y+30)] <- subset
        
      }
    
    }
  }
  
  return(outputMask)
}