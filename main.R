library(EBImage)
library(glcm)

source("createArtifactMask.R")
source("darkLineMask.R")

# read in the background frame
frame01 <- rotate(readImage("examples/set_1/frame01.tif"),0.5)@.Data

artifactMask <- createArtifactMask(frame01)
artifactMask <- artifactMask < 1 # invert

# Save it
writeImage(artifactMask, "examples/artifactMask.tiff")




frame02 <- rotate(readImage("examples/set_1/frame02.tif"),0.5)@.Data

# generate a line mask
lineMask <- darkLineMask(frame02)





# TESTING GLCM
frame.1 <- rotate(readImage("data/frame1.tif"),0.5)
frame.3 <- rotate(readImage("data/frame3.tif"),0.5)

a <- glcm(frame.3@.Data, n_grey=30, window=c(5,5))

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
c <- b*(frame.3@.Data<0.5)
c[is.na(c)] <- 0
c[c > 1] <- 1


kern <- makeBrush(7, shape='disc')
e <- dilateGreyScale(c, kern)
e <- erodeGreyScale(e, kern)
e <- e > 0.64
 
e[mask>0] <- 0

f <- removeBlobs(e<1, 150)
g <- removeBlobs(f<1, 150)



# few bacteria
frame.1[mask>0] <-1
a <- darkLineMask(frame.1)@.Data
a[a==1] <- NA

b <- glcm(a, n_grey=30, window=c(5,5), min_x=0, max_x=1, na_opt="any")

c <- b[,,3]*(frame.3@.Data<0.5)

d <- c < 0.2
d[is.na(d)] <- 0

kern <- makeBrush(7, shape='disc')
e <- dilateGreyScale(d, kern)
e <- erodeGreyScale(e, kern)

f <- removeBlobs(e, 75)




# 
# dflo <- makeBrush(15, shape="disc", step=FALSE)^2
# flo <- flo/sum(flo)
# d <- filter2(c, flo)
# d <- (d-frame.bg@.Data)*2 
# d[d<0] <- 0
# d[d>1] <- 1
# 
# 
# e <- frame.3@.Data-d
# 
# d[d<0] <- 0
# d[d>1] <- 1
# 
# 
# 
# d <- (c > 0.5)
# # d[mask > 0] <- 0






# flo <- makeBrush(15, shape="disc", step=FALSE)^2
# flo <- flo/sum(flo)
# d <- filter2(c, flo)
# 
# e <- (d^1.5)
# e[mask>0] <- 0

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
      m1[x] <- 0
    }
  }
  return(m1)
}
