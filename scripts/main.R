library(EBImage)
library(glcm)
library(plyr)

source("scripts/createArtifactMask.R")
source("scripts/darkLineMask.R")
source("scripts/removeBlobs.R")
source("scripts/isolateBacteria.R")
source("scripts/getCentroids.R")
source("scripts/findSimilarGroups.R")


# read in the background frame
frame01 <- rotate(readImage("examples/set_3/frame01.tif"),0.5)

# artifactMask <- createArtifactMask(frame01@.Data)
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


test2 <- isolateBacteria(frame02@.Data)
test3 <- isolateBacteria(frame03@.Data)
test4 <- isolateBacteria(frame04@.Data)
test5 <- isolateBacteria(frame05@.Data)
test6 <- isolateBacteria(frame06@.Data)
test7 <- isolateBacteria(frame07@.Data)
test8 <- isolateBacteria(frame08@.Data)
test9 <- isolateBacteria(frame09@.Data)
test10 <- isolateBacteria(frame10@.Data)
test11 <- isolateBacteria(frame11@.Data)

c2 <- getCentroids(test2)
c3 <- getCentroids(test3)
c4 <- getCentroids(test4)
c5 <- getCentroids(test5)
c6 <- getCentroids(test6)
c7 <- getCentroids(test7)
c8 <- getCentroids(test8)

# Groups in common between a and b
g1 <- findSimilarGroups(c2,c3)
g2 <- findSimilarGroups(c3,c4)
g3 <- findSimilarGroups(c4,c5)
g4 <- findSimilarGroups(c5,c6)
g5 <- findSimilarGroups(c6,c7)
g6 <- findSimilarGroups(c7,c8)


# Updates a centroid dataframe with IDs that are determined to be
# the same from the previous frame
updateIDs <- function(c1, g) {

  # select columns that continue between frame1 & 2
  idList <- as.character(c1$id)
  id1 <- as.character(g$id1)
  id2 <- as.character(g$id2)
  c1$id <- as.character(lapply(idList, function(x) if(x %in% id2) id1[which(id2==x)] else x))
  
  return(c1)
}
    


# update the output with a new timestep and 
updateOutput <- function(c1, g) {

  # find 
  newIDs <- c1[!(c1$id %in% colnames(output)),]
  
  # Create empty dataframe for new IDs
  newIDs.df <- data.frame(t(data.frame(rep(NA,dim(newIDs)[[1]]))))
  colnames(newIDs.df) <- newIDs$id
  
  # Bind current output with new IDs
  allIDs.df <- cbind(output, newIDs.df)
  
  # Create a new row with all of the new values
  newRow <- data.frame(t(data.frame(c1$size, row.names=c1$id)))
  
  # Add the new row with rbind.fill, which replaces missing values with NA
  return(rbind.fill(output, newRow))

}





# Initialize output
c2 <- getCentroids(test2)
output <- data.frame(t(data.frame(c2$size,row.names=c2$id)))

# Step1
c3 <- getCentroids(test3)
g1 <- findSimilarGroups(c2,c3)
c3 <- updateIDs(c3, g1)
output <- updateOutput(c3, g1)

# Step2
c4 <- getCentroids(test4)
g2 <- findSimilarGroups(c3,c4)
c4 <- updateIDs(c4, g2)
output <- updateOutput(c4, g2)

# Step3
c5 <- getCentroids(test5)
g3 <- findSimilarGroups(c4,c5)
c5 <- updateIDs(c5, g3)
updateOutput(c5, g3)





### TEST visually looking at continuations

m1 <- test2 %in% g1$b1
m1 <- matrix(m1, nrow=2149, ncol=1998)
m2 <- test3 %in% g1$b2
m2 <- matrix(m2, nrow=2149, ncol=1998)

m3 <- test3 %in% g2$b1
m3 <- matrix(m3, nrow=2149, ncol=1998)
m4 <- test4 %in% g2$b2
m4 <- matrix(m4, nrow=2149, ncol=1998)

display(m1)
display(m2)
display(m3)
display(m4)

