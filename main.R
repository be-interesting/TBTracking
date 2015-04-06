library(EBImage)
library(glcm)

source("createArtifactMask.R")
source("darkLineMask.R")

# read in the background frame
frame01 <- rotate(readImage("examples/set_1/frame01.tif"),0.5)@.Data

artifactMask <- createArtifactMask(frame01)
# artifactMask <- artifactMask < 1 # invert

# Save it
# writeImage(artifactMask, "examples/artifactMask.tiff")




frame02 <- rotate(readImage("examples/set_1/frame02.tif"),0.5)@.Data
frame03 <- rotate(readImage("examples/set_1/frame03.tif"),0.5)@.Data
frame04 <- rotate(readImage("examples/set_1/frame04.tif"),0.5)@.Data
frame05 <- rotate(readImage("examples/set_1/frame05.tif"),0.5)@.Data

# generate a line mask
lineMask <- darkLineMask(frame02)





# TESTING GLCM

a <- glcm(frame02, n_grey=30, window=c(5,5))

display(a[,,1])
display(a[,,2])
display(a[,,3])
display(a[,,4])
display(a[,,5])
display(a[,,6])
display(a[,,7])
display(a[,,8])


# lots of bacteria
b <- a[,,4]
c <- b*(frame02<0.5)
c[is.na(c)] <- 0
c[c > 1] <- 1

kern <- makeBrush(7, shape='disc')
e <- dilateGreyScale(c, kern)
e <- erodeGreyScale(e, kern)
e <- e > 0.62
 
e[artifactMask>0] <- 0
e[lineMask==1] <- 0

f <- removeBlobs(e<1, 150)
g <- removeBlobs(f<1, 150)






# few bacteria
frame02[artifactMask>0] <- 1
frame02[lineMask==1] <- 1
a <- frame02
a[a==1] <- NA

b <- glcm(a, n_grey=30, window=c(5,5), min_x=0, max_x=1, na_opt="any")

c <- b[,,3]*(frame02<0.5)

d <- c < 0.2
d[is.na(d)] <- 0

kern <- makeBrush(7, shape='disc')
e <- dilateGreyScale(d, kern)
e <- erodeGreyScale(e, kern)

f <- removeBlobs(e, 75)








e <- d < 1

kern <- makeBrush(5, shape='disc')
# e <- dilateGreyScale(d, kern)
e <- erodeGreyScale(e, kern)
# 

f <- bwlabel(e)@.Data
g <- f



h <- erodeGreyScale(g<1, kern)
h <- bwlabel(h)@.Data
aa <- h



removeBlobs <- function(m, size) {
  m <- bwlabel(m)
  m1 <- m
  for (i in 1:max(m)) {
    x <- m == i
    if (sum(x) < size) {
      print(i)
      m1[x] <- 0
    }
  }
  return(m1)
}


# isolateEarly1 <- function(m) {

  a <- glcm(m, n_grey=20, window=c(5,5), min_x=0, max_x=0.5, na_opt="any")
  
  # few bacteria
  b <- a[,,4]
  c <- b*(frame02<0.3)
  c[is.na(c)] <- 0
  c[c > 1] <- 1
  
  kern <- makeBrush(3, shape='disc')
  e <- dilateGreyScale(c, kern)
  e <- erodeGreyScale(e, kern)
  e <- e > 0.68
  
  e[artifactMask>0] <- 0
  e[lineMask==1] <- 0
  
  f <- removeBlobs(e<1, 150)
  g <- removeBlobs(f<1, 150)
  
  kern <- makeBrush(3, shape='disc')
  h <- erodeGreyScale(g, kern)

}

# This is better
isolateEarly2 <- function(m) {
  
  lineMask <- darkLineMask(m)
  
  a <- glcm(m, n_grey=20, window=c(5,5), min_x=0, max_x=0.5, na_opt="any")
  
  # few bacteria
  b <- a[,,4]
  c <- b*(m<0.3)
  c[is.na(c)] <- 0
  c[c > 1] <- 1
  
  kern <- makeBrush(3, shape='disc')
  e <- dilateGreyScale(c, kern)
  e <- erodeGreyScale(e, kern)
  e <- e > 0.68
  
  e[artifactMask>0] <- 0
  e[lineMask==1] <- 0
  
  f <- removeBlobs(e<1, 25)
  g <- removeBlobs(f<1, 25)
  
  kern <- makeBrush(3, shape='disc')
  h <- dilateGreyScale(g, kern)
  
  return(h)
  
}

test1 <- isolateEarly2(frame02)
test2 <- isolateEarly2(frame03)
test3 <- isolateEarly2(frame04)
test4 <- isolateEarly2(frame05)
