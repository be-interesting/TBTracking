library(EBImage)
library(glcm)

source("createArtifactMask.R")
source("darkLineMask.R")

# read in the background frame
frame01 <- rotate(readImage("examples/set_3/frame01.tif"),0.5)

artifactMask <- createArtifactMask(frame01@.Data)
# artifactMask <- artifactMask < 1 # invert

# Save it
writeImage(artifactMask, "examples/artifactMask.tiff")
artifactMask <- readImage("examples/artifactMask.tiff")

# Load sample data
frame02 <- rotate(readImage("examples/set_3/frame02.tif"),0.5)
frame03 <- rotate(readImage("examples/set_3/frame03.tif"),0.5)
frame04 <- rotate(readImage("examples/set_3/frame04.tif"),0.5)
frame05 <- rotate(readImage("examples/set_3/frame05.tif"),0.5)
frame06 <- rotate(readImage("examples/set_3/frame06.tif"),0.5)
frame07 <- rotate(readImage("examples/set_3/frame07.tif"),0.5)
frame08 <- rotate(readImage("examples/set_3/frame08.tif"),0.5)
frame09 <- rotate(readImage("examples/set_3/frame09.tif"),0.5)
frame10 <- rotate(readImage("examples/set_3/frame10.tif"),0.5)
frame11 <- rotate(readImage("examples/set_3/frame11.tif"),0.5)





# Remove all blobs under a certain size threshold
removeBlobs <- function(m, size) {
  # Label blobs
  m <- bwlabel(m)
  # Save dimensions
  dims <- dim(m)
  # Unravel matrix and count the occurances of each label
  sorted <- sort(table(as.numeric(m)))
  # Labels which are under the threshold
  small <- as.numeric(names(sorted[sorted<size]))
  # Remove pixels that fall into undersized labels
  m[m %in% small] <- 0
  # Turn back into matrix
  m <- matrix(m, nrow=dims[[1]], ncol=dims[[2]])
  return(m)
}



# Returns a black and white mask of where bacteria can be found
isolateEarly <- function(m) {
  
  # Create mask of where dark lines are unlikely to contain bacteria
  lineMask <- darkLineMask(m)
  
  # Create glcm masks
  a <- glcm(m, n_grey=18, shift=list(c(0,1),c(1,0),c(1,1)), 
            window=c(3,3), min_x=0, max_x=0.6,
            statistics=c("dissimilarity"))

  # Dark out background and apply masks
  b <- (a[,,1]-m)
  b[artifactMask>0] <- 0
  b[lineMask==1] <- 0
  
  # remove pixels over 50% value
  c <- b > 0.5
  
  # Mild dilate/erode to close small gaps
  kern <- makeBrush(3, shape='disc')
  d <- dilateGreyScale(c, kern)
  d <- erodeGreyScale(d, kern)
  
  # Remove small blobs
  e <- removeBlobs(d, 50)
  
  # run dilate, mask, and remove smaller blobs
  f <- dilateGreyScale(e, kern)
  g <- f * (m < 0.45)
  h <- removeBlobs(g, 35)  

  return(h)
  
}



test2 <- isolateEarly(frame02@.Data)
test3 <- isolateEarly(frame03@.Data)
test4 <- isolateEarly(frame04@.Data)
test5 <- isolateEarly(frame05@.Data)
test6 <- isolateEarly(frame06@.Data)
test7 <- isolateEarly(frame07@.Data)
test8 <- isolateEarly(frame08@.Data)
test9 <- isolateEarly(frame09@.Data)
test10 <- isolateEarly(frame10@.Data)
test11 <- isolateEarly(frame11@.Data)








# 
# 
# ### TEST
# ### Labeling images
# 
# df <- data.frame(x=c(),y=c(),size=c(),stringsAsFactors=FALSE)
# x <- c()
# y <- c()
# z <- c()
# 
# for (i in 1:max(m)) {
#   m <- bwlabel(m)
#   m1 <- m == i
#   t <- which(m1, arr.ind=T)
#   x <- c(x,mean(t[,1]))
#   y <- c(y,mean(t[,2]))
#   z <- c(z,sum(m1))
# }
# 
# 
# png("test.png",width=2149,height=1998)
# par(mar=c(0,0,0,0), oma=c(0,0,0,0))
# 
# plot(x, y, pch=19, cex=z/max(z), type="n", axes=T, 
#      ylim=c(1998,1),xlim=c(1,2149),
#      xaxt="n", yaxt='n', xaxs='i', yaxs='i')
# 
# for (i in 1:max(m)) {
#   text(x[[i]],y[[i]],z[[i]],cex=1)
# }
# 
# dev.off()
# 
# 
# # [,,1] [,,2] [,,3] [,,4] are RGB Bl 
# labels <- readImage("test.png")
# labels <- 1-labels@.Data[,,1]
# 
# 
# display(labels + frame02@.Data)

