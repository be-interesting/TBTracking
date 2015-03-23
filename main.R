library(EBImage)

source("createFillCircle.R")
source("createArtifactMask.R")
source("darkLineMask.R")

# Load artifact mask TODO should this include removing dark lines?
# Align with background
# "Levels" algorithm to isolate bacteria as black pixels
# Statistically group black clumps together

# read in the background frame
# mean: 0.4575868
frame.bg <- rotate(readImage("data/background.tif"),0.5)

# create a background artifact mask
artifactMask <- createArtifactMask(frame.bg)
artifactMask <- artifactMask < 1 # invert
# Save it
writeImage(artifactMask, "artifactMask.tiff")


# read in a sample frame of data
# mean: 0.4658735
frame.1 <- rotate(readImage("data/frame1.tif"),0.5)

# generate a line mask
lineMask <- darkLineMask(frame.1)



############################################################################
###################### image value processing example ######################
############################################################################

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

##################################################################
##################################################################
##################################################################


##################################################################
################ texture processing example ######################
##################################################################

frame.3 <- rotate(readImage("data/frame3.tif"),0.5)
mask <- rotate(readImage("artifactMask.tiff"),0.5)
frame.3[mask>0] <- 1

s1 <- frame.3[50:150, 650:750] # all bacteria
s2 <- frame.3[450:550, 150:250] # no bacteria
s3 <- frame.3[400:500, 700:800] # some of both


b1 <- round(s1*10)/10
b2 <- round(s2*10)/10
b3 <- round(s3*10)/10

# s1.diff <- apply(s1, 1, function(x) abs(diff(x))) 
# s2.diff <- apply(s2, 1, function(x) abs(diff(x)))
# s3.diff <- apply(s3, 1, function(x) abs(diff(x)))
# 
# s1.diff[s1.diff>0.15] <- 0
# s2.diff[s2.diff>0.15] <- 0
# s3.diff[s3.diff>0.15] <- 0
# 
# test <- kmeans(s1, 5)$centers
# 
# tx <- c()
# ty <- c()
# 
# # https://courses.cs.washington.edu/courses/cse576/book/ch7.pdf
# # Texture energy analysis
# 
# 
# # empty plot for checking rows
# plot(0,0,xlim=c(0,100), ylim=c(0,1))
# 
# apply(b1, 1, function(x) points(diff(x), pch=19))
# apply(b2, 1, function(x) points(diff(x), pch=19, col="red"))
# 
# mean(apply(b1, 1, function(x) mean(abs(diff(x)))))
# mean(apply(b3, 1, function(x) mean(abs(diff(x)))))


c1 <- coocc(s1@.Data)
c2 <- coocc(s2@.Data)
c3 <- coocc(s3@.Data)

# energy
e1 <- sum(c1[[1]]^2)
e2 <- sum(c2[[1]]^2)

# Homogeneity
homogeneity <- function(m) {
  h = 0
  for(i in 1:dim(m)[[1]]) {
    for(j in 1:dim(m)[[2]]) {
      s <- m[i,j] / (1 + abs(i - j))
      h <- h + s
    }
  }
  return(h)
}

apply(b2, 1, function(x) points(diff(x), pch=19, col="red"))

apply(s2.diff[0:10,], 1, function(x) lines(x, col="red"))
apply(s2.diff, 1, function(x) lines(x, col="blue"))


lines(s2.diff[,1])



homo <- matrix(0,200,206)
ener <- matrix(0,170,170)
entr <- matrix(0,170,170)

frame.3d <- frame.3@.Data
cpy <- frame.3d

for (x in 151:((dim(frame.3d)[[1]]-8)/8)) {
  x1 <- 1 + 8*(x-1)
  for (y in 12:((dim(frame.3d)[[2]]-8)/8)) {
    print(paste(x,y))
    y1 <- 1 + 8*(y-1)
    ss <- frame.3d[x1:(x1+8),y1:(y1+8)]
    c <- coocc(ss)
    ss[ss==1] <- NA
    m <- mean(ss, na.rm=TRUE)
    homo[x,y] <- homogeneity(c) * m
#     ener[x,y] <- energy(c) * m
#     entr[x,y] <- entropy(c) * m
  }
}

