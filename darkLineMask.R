# This function finds very dark lines, isolates the parts that don't
# appear to be covering bacteria, and returns a mask of that area.
# This technique certianly isn't perfect but it is a start

darkLineMask <- function(df) {
  
  # Iterate through the imagine in 15 pixel rows and find the mean of each row
  meanValue <- c()
  for(y in seq(15,(dim(df)[[2]]-15),2)) {
    sample <- df[ ,y:(y+14)]
    meanValue <- c(meanValue, mean(sample))
  }
  
  # Search through the mean values in groups of 100 and save the index of the
  # max value IF the value is over 0.7
  indices <- c()
  for(i in seq(1,length(meanValue)-100,by=100)) {
    subset <- meanValue[i:(i+99)]
    if(max(subset) > 0.7) {
      indices <- c(indices,(which.max(subset)+i)*2)
    }
  }
  
  # For each of these high points, mask the background and save the bacter
  for (i in indices) {
    
    y1 <- i-11
    y2 <- i+13
    
    sample <- df[ ,y1:y2]
    
    a <- glcm(sample, shift=list(c(-1,0),c(1,0)), window=c(3,3), min_x=0, max_x=0.5, na_opt="ignore")
    b <- a[,,3] < 0.45

    kern <- makeBrush(7, shape='disc')
    dilated <- dilateGreyScale(b, kern)
    eroded <- erodeGreyScale(dilated, kern)

    mask <- eroded
    
    df[ ,y1:y2] <- mask * df[ ,y1:y2]
#     df[ ,(i-1):(i+13)] <- mask * df[ ,(i-1):(i+13)]

  }

  return(df==0)

}