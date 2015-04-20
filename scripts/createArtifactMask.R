# Creates white pixel mask which removes artifacts from dataset.
# Artifacts include grided black circles and large squares which
# designate slide number.

# Assume the background will be passed in
createArtifactMask <- function(bg) {
  
  # copy bg so we don't edit the original
  output <- bg
  
  # size of raster
  dim.x <- dim(bg)[[1]]
  dim.y <- dim(bg)[[2]]
  
  # What percent of this matrix is "dark"
  percentDark <- function(m) {
    return(sum(m<0.5, na.rm=TRUE) / (dim(m)[[1]] * dim(m)[[2]]))
  }
  
  # is a significantly darker than b?
  isDarker <- function(a,b) {
    return(percentDark(a) - percentDark(b) > 0.28)
  }

  

  

  
  
  
  
  ####################################
  ####### REMOVING THE SQUARES #######
  ####################################
  
  # Find areas of significant change to search
  sq.row <- diff(apply(bg,2,sd) > 0.2)
  sq.row.start <- which(sq.row==1)
  
  sq.col <- diff(apply(bg,1,sd) > 0.18)
  sq.col.start <- which(sq.col==1)
  
  # Expand the search range a little bit
  xIndices <- unlist(lapply(sq.col.start, function(x) seq(x-3, x+3, by=3)))
  yIndices <- unlist(lapply(sq.row.start, function(x) seq(x-3, x+3, by=3)))
  
  # first remove the squares. Without those it's easier to narrow down
  # the search space for circles
  for (x in xIndices) {
    for (y in yIndices) {
     
      # size of squares
      sq.x <- 230
      sq.y <- 230 
      
      # make sure we aren't checking outside the canvas
      if(x < (dim.x-sq.x) & y < (dim.y-sq.y)) {
        
        # get current subset
        subset <- bg[x:(x+(sq.x-1)),y:(y+sq.y-1)]
      
        # get y strips for comparison
        y11 <- subset[0:20, ]
        y12 <- subset[20:40, ]
        y21 <- subset[(sq.x-20):sq.x, ]
        y22 <- subset[(sq.x-40):(sq.x-20), ]
        
        # get x strips for comparison
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
        
        # rules that squares tend to follow
        if (isDarker(y12,y11) & isDarker(y22,y21) 
            & isDarker(x12, x11) & isDarker(x22, x21)
            & percentDark(center) < 0.3) {
          # create a square mask. the mask is a little smaller than the sample
          # because I only want to remove the dark parts
          sq.mask <- matrix(0, sq.x, sq.y)
          sq.mask[1:10, ] <- 1
          sq.mask[(sq.x-10):sq.x, ] <- 1
          sq.mask[,1:10] <- 1
          sq.mask[,(sq.y-10):sq.y] <- 1
          output[x:(x+(sq.x-1)),y:(y+sq.y-1)] <- output[x:(x+(sq.x-1)),y:(y+sq.y-1)] * sq.mask
        }
        
      }
      
    }
  }
  
  # create a texture mask
  # use the contrast statistic (layer 5)
  a <- glcm(bg, n_grey=10)[,,3]
  
  # dilate and erode to close gaps in the middle of circles
  kern <- makeBrush(23, shape='disc')
  b <- dilateGreyScale(a<0.5, kern)
  c <- erodeGreyScale(b, kern)
  
  # add the square mask
  c[output==0] <- 1
  
  # close gaps between partial circles and squares
  kern <- makeBrush(15, shape='disc')
  d <- dilateGreyScale(c, kern)
  e <- erodeGreyScale(d, kern)
  
  # Expand the circles a bit
  kern <- makeBrush(3, shape='disc')
  e <- dilateGreyScale(e, kern)
  
  f <- e
  ### REMOVE WHITE LINES ###
  for(y in seq(5,dim.y-20,by=2)) {
    sample <- e[,y:(y+5)]
    if( sum(sample) / (dim.y*6) > 0.8) {
      f[,(y-5):(y+20)] <- 0
    }
  }
  
  return(f)
  
}