# Creates white pixel mask which removes artifacts from dataset.
# Artifacts include grided black circles and large squares which
# designate slide number.

# Assume the background will be passed in
createArtifactMask <- function(bg) {
  
  # Extract pixel data for speed's sake
  bg.data <- bg@.Data
  sq.data <- bg@.Data
  cr.data <- bg@.Data
  
  # Get dimensions
  dim.x <- dim(bg.data)[[1]]
  dim.y <- dim(bg.data)[[2]]
  
  # Set mask to matrix of dim xy
  output <- matrix(1, dim.x, dim.y)
  
  # TEST
  # Isolate the y ranges to search to speed things up
  highs <- c(121, 215, 303, 393, 487, 575, 667, 755, 849, 935, 1025, 1112, 1205, 1295, 1385, 1475, 1565)
  # Add 20 indices in between each element of highs to search
  yIndices <- unlist(lapply(highs, function(x) seq(x, x+20)))
  

  # Loop through the y indices
  for (y in yIndices) {
    
    # For each y value, search through all of the x values and find similar areas
    for (x in seq(1, dim.x-31, 4)) {
      
      # Arbitrary threshold
      thr1 <- 0.039
      thr2 <- 0.041
      
      # Get subset of original
      subset <- bg.data[x:(x+30),y:(y+30)]
      
      dx.beg <- mean(abs(diff(subset[15,0:10])))
      dx.mid <- mean(abs(diff(subset[15,12:18])))
      dx.end <- mean(abs(diff(subset[15,20:30])))
      
      dy.beg <- mean(abs(diff(subset[15,0:10])))
      dy.mid <- mean(abs(diff(subset[15,12:18])))
      dy.end <- mean(abs(diff(subset[15,20:30])))
      
      if(dx.mid < 0.039 & dy.mid < 0.039 & dx.beg - dx.end < 0.039 & dy.beg - dy.end < 0.5 & dx.beg > 0.039
         & dx.end > 0.039 & dy.beg > 0.039 & dx.end > 0.039) {
          # mask subset with white circle
          subset <- cr.data[x:(x+30),y:(y+30)] * createFillCircle(31, 31, 9)
          output[x:(x+30),y:(y+30)] <- output[x:(x+30),y:(y+30)] * createFillCircle(31, 31, 9)
      }
      

      sq.x <- 230
      sq.y <- 230 
      
      if (x < dim.x - sq.x & y < dim.y - sq.y) {
        
        # Get subset of original
        subset <- bg.data[x:(x+(sq.x-1)),y:(y+sq.y-1)]
        
        perDark <- function(m) {
          return(sum(m<0.5, na.rm=TRUE) / (dim(m)[[1]] * dim(m)[[2]]))
        }
        
        isDarker <- function(a,b) {
          return(perDark(a) - perDark(b) > 0.28)
        }
        
        # compare y strips
        y11 <- subset[0:20, ]
        y12 <- subset[20:40, ]
        y21 <- subset[(sq.x-20):sq.x, ]
        y22 <- subset[(sq.x-40):(sq.x-20), ]
        
        # compare x strips
        x11 <- subset[ ,0:20]
        x12 <- subset[ ,20:40]
        x21 <- subset[ ,(sq.x-20):sq.x]
        x22 <- subset[ ,(sq.x-40):(sq.x-20)]
        
        # get center
        cx1 <- round(sq.x/2 - 10)
        cx2 <- round(sq.x/2 + 10)
        cy1 <- round(sq.y/2 - 10)
        cy2 <- round(sq.y/2 + 10)
        center <- subset[cx1:cx2,cy1:cy2]
        
        
        if (isDarker(y12,y11) & isDarker(y22,y21) & isDarker(x12, x11) & isDarker(x22, x21)
            & perDark(center) < 0.3) {
          # mask subset with square
          sq.mask <- matrix(0, sq.x, sq.y)
          sq.mask[1:6, ] <- 1
          sq.mask[(sq.x-6):sq.x, ] <- 1
          sq.mask[,1:6] <- 1
          sq.mask[,(sq.y-6):sq.y] <- 1
          output[x:(x+(sq.x-1)),y:(y+sq.y-1)] <- output[x:(x+(sq.x-1)),y:(y+sq.y-1)] * sq.mask
        }
      }
        
    }
  }


  return(output)
}