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
  
  sq.col <- diff(apply(bg,1,sd) > 0.17)
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
            & percentDark(center) < 0.4) {
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
  kern <- makeBrush(5, shape='disc')
  e <- dilateGreyScale(e, kern)
  
  f <- e
  ### REMOVE WHITE LINES ###
  for(y in seq(5,dim.y-20,by=2)) {
    sample <- e[,y:(y+5)]
    if( sum(sample) / (dim.x*6) > 0.8) {
      f[,(y-5):(y+20)] <- 0
    }
  }
  
  return(f)
  
}




# Input: x and y dimension of matrix, circle radius
# Returns a x by y matrix with a circle of radius r at the center
# Values inside of the circle are 0, outside are 1
createFillCircle <- function(dim.x, dim.y, r) {
  
  # Get x and y points of circle
  x1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dim.x/2 + (r * cos(a)))))
  y1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dim.y/2 + (r * sin(a)))))
  
  # Put points in dataframe
  points <- data.frame(x1, y1)
  
  # Initialize empty circle
  circle <- matrix(0, dim.x, dim.x)
  
  for (x in 1:dim.x) {
    # Y values with the current x value
    yRange <- points[points$x1==x,]$y1
    if(length(yRange) < 1) {
      # If there are no y values, entire row is 1
      circle[x,] <- 1
    } else {
      # If there are, fill area outside of that range with 1s
      yRange <- range(yRange)
      circle[x,1:yRange[[1]]] <- 1
      circle[x,yRange[[2]]:dim.y] <- 1
    }
  }
  
  return(circle)
  
}




# This function finds very dark lines, isolates the parts that don't
# appear to be covering bacteria, and returns a mask of that area.
# This technique certianly isn't perfect but it is a start

darkLineMask <- function(df) {
  
  lineMask <- df
  lineMask.inverse <- df
  
  # Iterate through the imagine in 15 pixel rows and find the mean of each row
  meanValue <- c()
  for(y in seq(1,(dim(df)[[2]]-5),1)) {
    sample <- df[ ,y:(y+5)]
    meanValue <- c(meanValue, mean(sample))
  }
  
  d.start <- which(diff(meanValue < 0.55) > 0) - 5
  d.end <- which(diff(meanValue < 0.55) < 0) + 5
  d.range <- cbind(d.start, d.end)
  
  l.start <- which(diff(meanValue > 1.45) > 0)
  l.end <- which(diff(meanValue > 1.45) < 0) + 10
  l.range <- cbind(l.start, l.end)
  
  if((length(d.range) + length(l.range)) < 1) {
    return(list(
      normal=df==0,
      inverse=df==1
    ))
  }
  
  # For each of these low points, mask the background and save the bacteria
  for (i in 1:dim(d.range)[[1]]) {
    
    y1 <- d.range[i,1]
    y2 <- d.range[i,2]
    
    sample <- df[ ,y1:y2]
    
    a <- glcm(sample, shift=list(c(-1,0),c(1,0)), window=c(3,3), min_x=0, max_x=1, na_opt="ignore")
    b <- (a[,,3] < 0.4)
    b[sample > 0.65] <- 0
    
    kern <- makeBrush(9, shape='disc')
    dilated <- dilateGreyScale(b, kern)
    kern <- makeBrush(7, shape='disc')
    eroded <- erodeGreyScale(dilated, kern)
    
    eroded[sample > 0.55] <- 0
    
    mask <- eroded
    
    lineMask[ ,y1:y2] <- mask * lineMask[ ,y1:y2]
    lineMask.inverse[ ,y1:y2] <- !mask * lineMask.inverse[ ,y1:y2]
    
  }
  
  for (i in 1:dim(l.range)[[1]]) {
    
    y1 <- l.range[i,1]
    y2 <- l.range[i,2]
    
    sample <- df[ ,y1:y2]
    #     
    a <- glcm(sample, shift=list(c(1,0)), window=c(3,3), min_x=0, max_x=1, na_opt="ignore",
              statistics=c("homogeneity"))
    
    mask <- (a[,,1] * sample) < 0.4
    mask[is.na(mask)] <- 0
    #     
    #     b <- a[,,3] < 0.3
    #     
    #     kern <- makeBrush(7, shape='disc')
    #     dilated <- dilateGreyScale(b, kern)
    #     eroded <- erodeGreyScale(dilated, kern)
    #     
    #     eroded[sample > 0.6] <- 0
    #     
    #     mask <- eroded
    
    lineMask[ ,y1:y2] <- mask * lineMask[ ,y1:y2]
    lineMask.inverse[ ,y1:y2] <- !mask * lineMask.inverse[ ,y1:y2]
    
  }
  
  return(list(
    normal=lineMask==0,
    inverse=lineMask.inverse==0
  ))
  
}