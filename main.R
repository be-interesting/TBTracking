library(EBImage)
library(glcm)

source("createArtifactMask.R")
source("darkLineMask.R")

# read in the background frame
frame01 <- rotate(readImage("examples/set_3/frame01.tif"),0.5)

artifactMask <- createArtifactMask(frame01@.Data)
# artifactMask <- artifactMask < 1 # invert

# Save it
# writeImage(artifactMask, "examples/artifactMask.tiff")
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
  
  clock <- proc.time()
  
  
  
  # Create mask of where dark lines are unlikely to contain bacteria
  lineMask <- darkLineMask(m)
  
  print("LineMask finished")
  print(proc.time() - clock)
  
  # Create glcm masks
  a <- glcm(m, n_grey=20, window=c(5,5), min_x=0, max_x=0.5, na_opt="any")
  
  print("glcm finished")
  print(proc.time() - clock)
  
  # few bacteria
  b <- a[,,4]
  c <- b*(m<0.3)
  c[is.na(c)] <- 0
  c[c > 1] <- 1
  
  kern <- makeBrush(3, shape='disc')
  d <- dilateGreyScale(c, kern)
  d <- erodeGreyScale(d, kern)
  e <- d > 0.68

  # Apply artifact mask for experiment and linemask for this frame
  e[artifactMask>0] <- 0
  e[lineMask==1] <- 0
  
  # Remove small blobs
  f <- removeBlobs(e<1, 25)
  g <- removeBlobs(f<1, 25)
  
  print("Blob removal finished")
  print(proc.time() - clock)
  
  # Slightly dilate to join fragments together
  kern <- makeBrush(3, shape='disc')
  h <- dilateGreyScale(g, kern)
  
  return(h)
  
}



test1 <- isolateEarly(frame02@.Data)
test2 <- isolateEarly(frame03@.Data)
test3 <- isolateEarly(frame04@.Data)
test4 <- isolateEarly(frame05@.Data)
test5 <- isolateEarly(frame06@.Data)
test6 <- isolateEarly(frame07@.Data)
test7 <- isolateEarly(frame08@.Data)
test8 <- isolateEarly(frame09@.Data)
test9 <- isolateEarly(frame10@.Data)
test10 <- isolateEarly(frame11@.Data)




### TEST
### Labeling images

df <- data.frame(x=c(),y=c(),size=c(),stringsAsFactors=FALSE)
x <- c()
y <- c()
z <- c()

for (i in 1:max(m)) {
  m <- bwlabel(m)
  m1 <- m == i
  t <- which(m1, arr.ind=T)
  x <- c(x,mean(t[,1]))
  y <- c(y,mean(t[,2]))
  z <- c(z,sum(m1))
}


png("test.png",width=2149,height=1998)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))

plot(x, y, pch=19, cex=z/max(z), type="n", axes=T, 
     ylim=c(1998,1),xlim=c(1,2149),
     xaxt="n", yaxt='n', xaxs='i', yaxs='i')

for (i in 1:max(m)) {
  text(x[[i]],y[[i]],z[[i]],cex=1)
}

dev.off()


# [,,1] [,,2] [,,3] [,,4] are RGB Bl 
labels <- readImage("test.png")
labels <- 1-labels@.Data[,,1]


display(labels + frame02@.Data)

