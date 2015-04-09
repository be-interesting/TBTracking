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



# This is better
isolateEarly <- function(m) {
  
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

