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
  
    # isolate darkest area
    sample <- df[ ,(i-7):(i+7)]
    
    bright <- sample > 0.5
    kern = makeBrush(7, shape='disc')
    bright = dilateGreyScale(bright, kern)
    sample[bright > 0] <- 0.3
#     test1 <- c()
#     test2 <- c()
#     test3 <- c()
    
    mask <- sample
    
    for(x in 1:(dim(sample)[[1]]-5)) {
      slice <- sample[x:(x+5), ] * 2
#       test1 <- c(test1, min(slice))
#       test2 <- c(test2, mean(slice))
#       test3 <- c(test3, norm(diff(slice)))
      if (norm(diff(slice)) > 0.3) {
        mask[x:(x+5), ] <- 1
      }
    }

    mask[mask<1] <- 0
    

    
    
    
    
#     # brighten so bacteria stand out more
#     
#     # find and expand bright area
#     bright <- sample > 0.5
#     kern <- makeBrush(3, shape='disc')
#     bright <- dilateGreyScale(bright, kern)
#     sample[bright > 0] <- 0.3
#     
#     a <- glcm(sample, min_x=0, max_x=0.25)[,,3]
#     
#     a <- a * sample < 0.1
#     
#     kern <- makeBrush(7, shape='disc')
#     dilated <- dilateGreyScale(a, kern)
#     eroded <- erodeGreyScale(dilated, kern)
    
    df[ ,(i-7):(i+7)] <- mask * df[ ,(i-7):(i+7)]
    df[ ,(i-1):(i+13)] <- mask * df[ ,(i-1):(i+13)]

  }
  
  df[df==0] <- 1

  return(df)

}