library(EBImage)

source("createFillCircle.R")
source("createArtifactMask.R")
source("darkLineMask.R")

# Load artifact mask TODO should this include removing dark lines?
# Align with background
# "Levels" algorithm to isolate bacteria as black pixels
# Statistically group black clumps together

# read in the background frame for testing
frame.bg <- rotate(readImage("data/background.tif"),0.5)
# mean: 0.4575868

frame.1 <- rotate(readImage("data/frame1.tif"),0.5)
# mean: 0.4658735

lineMask <- darkLineMask(frame.1)


# This definitely works
artifactMask <- createArtifactMask(frame.bg)
artifactMask <- artifactMask < 1 # invert
# Save it
writeImage(artifactMask, "artifactMask.tiff")

# rotate!

mask <- rotate(readImage("artifactMask.tiff"),0.5)
lineMask[mask > 0] <- 1

writeImage(lineMask, "data/frame1.masked.tif")

a <- lineMask

b <- (a < 0.33)[10:(dim(a)[[1]]-10),10:(dim(a)[[2]]-10)]

kern <- makeBrush(3, shape='disc')
c <- dilateGreyScale(b, kern)

d <- bwlabel(c)@.Data

e <- d

sizes <- c()
for(i in 1:max(d)) {
  print(i)
  t <- d == i
  size <- sum(t)
  sizes <- c(sizes, size)
  if(size < 70) e[t] <- 0
}

f <- bwlabel(e)@.Data






frame.1 <- readImage("data/frame1-post.tif")

a <- bwlabel(frame.1)
a.d <- a@.Data

for(i in 1:max(a.d)) {
  print(i)
  t <- a.d == i
  size <- sum(t)
  if(size < 55) {
    a.d[t] <- 0
  }
}

kern = makeBrush(3, shape='disc')
a.d = dilateGreyScale(a.d, kern)



frame.1[a.d > 0] <- 1




end <- rotate(readImage("data/frame3.tif"), 0.5)
end[mask > 0] <- 1

