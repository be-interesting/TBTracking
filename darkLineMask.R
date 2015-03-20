# This function finds very dark lines, isolates the parts that don't
# appear to be covering bacteria, and returns a mask of that area.
# This technique certianly isn't perfect but it is a start

darkLineMask <- function(df) {
  
  # Find the index of the darkest region. Search through the image in
  # 15 pixel rows and return the darkest area
  darkest <- list(value=1, index=1)
  
  for(y in seq(15,(dim(df)[[2]]-15),2)) {
    sample <- df[ ,y:(y+14)]
    if (mean(sample) < darkest$value) {
      darkest$index <- y
      darkest$value <- mean(sample)
    } 
  }
  
  # isolate darkest area
  sample <- df[ ,darkest$index:(darkest$index+14)]@.Data
  
  # brighten so bacteria stand out more
  
  # find and expand bright area
  bright <- sample > 0.5
  kern = makeBrush(3, shape='disc')
  bright = dilateGreyScale(bright, kern)
  sample[bright > 0] <- 0.3

#   test1 <- c()
#   test2 <- c()
#   test3 <- c()
#   
  mask <- sample
  
  # smooooooth jazz
  # suspected bacteria columns are white
  for(x in 1:(dim(sample)[[1]]-5)) {
    slice <-sample[x:(x+5), ]
#     test1 <- c(test1, sd(slice))
#     test2 <- c(test2, mean(slice))
#     test3 <- c(test3, diff(slice))
    if (sd(slice) > 0.04) {
      mask[x:(x+5), ] <- 1
    }
  }
  
  # black out regions with no bacteria
  mask[mask<1] <- 0
  
  # hacky bad way of extending the mask to extra 3 rows
  df[ ,darkest$index:(darkest$index+14)] <- df[ ,darkest$index:(darkest$index+14)] * mask
  df[ ,(darkest$index + 3):(darkest$index+17)] <- df[ ,darkest$index:(darkest$index+14)] * mask
  
  df[df==0] <- 1

  return(df)

}