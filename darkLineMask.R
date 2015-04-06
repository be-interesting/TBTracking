# This function finds very dark lines, isolates the parts that don't
# appear to be covering bacteria, and returns a mask of that area.
# This technique certianly isn't perfect but it is a start

darkLineMask <- function(df) {
  
  # Find the index of the darkest region. Search through the image in
  # 15 pixel rows and return the darkest area
  darkest <- list(value=1, index=1)
  darks <- c()
  
  for(y in seq(15,(dim(df)[[2]]-15),2)) {
    sample <- df[ ,y:(y+14)]
    darks <- c(darks, mean(sample))
  }
  
  indices = c()
  for(i in seq(1,length(darks)-100,by=100)) {
    subset <- darks[i:(i+99)]
    if(max(subset) > 0.7) {
      indices <- c(indices,(which.max(subset)+i)*2)
    }
  }
  
  for (i in indices) {
  
    # isolate darkest area
    sample <- df[ ,(i-7):(i+11)]
    
    # brighten so bacteria stand out more
    
    # find and expand bright area
    bright <- sample > 0.5
    kern <- makeBrush(3, shape='disc')
    bright <- dilateGreyScale(bright, kern)
    sample[bright > 0] <- 0.3
    
    sample <- glcm(sample)[,,3]
    
    sample <- sample < 0.3
    
    kern <- makeBrush(7, shape='disc')
    dilated <- dilateGreyScale(sample, kern)
    eroded <- erodeGreyScale(dilated, kern)
    
    df[ ,(i-7):(i+11)] <- eroded * df[ ,(i-7):(i+11)]

  }
  
  df[df==0] <- 1

  return(df)

}