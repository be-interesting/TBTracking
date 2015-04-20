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