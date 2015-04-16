library(EBImage)
library(glcm)
library(plyr)
library(RColorBrewer)

source("scripts/createArtifactMask.R")
source("scripts/darkLineMask.R")
source("scripts/removeBlobs.R")
source("scripts/isolateBacteria.R")
source("scripts/getCentroids.R")
source("scripts/findSimilarGroups.R")
source("scripts/appendOutput.R")
source("scripts/updateCentroidIDs.R")


# read in the background frame
frame01 <- rotate(readImage("examples/set_3/frame01.tif"),0.5)

# artifactMask <- createArtifactMask(frame01@.Data)
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



### Accepts a list of preprocessed frames
### Returns a data frame of blob ids and pixel areas
main <- function(frames) {
  
  if (length(frames) < 1) {
    stop()
  }
  
  print("Processing first frame...")
  
  # Process first frame
  firstFrame <- frames[[1]]
  centroidsBefore <- getCentroids(firstFrame)
  
  # Initialize output
  output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))
  
  # Remove first frame
  frames[[1]] <- NULL
  
  index <- 0
  for (frame in frames) {
    index <- index + 1
    print(paste0("Processing frame ", index, " of ", length(frames)))
    
    centroidsAfter <- getCentroids(frame)
    
    # Find groups that are determined to be the same between the two frames
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter)
    
    # For those continued group, give them the proper ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    output <- appendOutput(centroidsAfter, groups, output)
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
  }
  
  return(output)
  
}


  

### TEST this makes a cool graph
test <- main(list(test2,test3,test4,test5))
test <- test[,order(test[3,], test[4,], test[5,], 
                    test[6,], test[7,], test[8,], test[9,], test[10,]) ]
test <- test[,!is.na(test[1,])]
cols <- brewer.pal(5,"Greens")
plot(0,0,log="y",xlim=c(1,100), ylim=c(log(min(test,na.rm=TRUE)),log(max(test, na.rm=TRUE))))
for (i in 1:dim(test)[[2]]) {
  subset <- test[,i]
  for (j in 1:length(subset)) {
    points(i, log(subset[[j]]), pch=19, cex=0.5, col=cols[[j]])
  }
}



### TEST a different kind of plot

plot(0,0,log="y",xlim=c(1,100), ylim=c(log(min(test,na.rm=TRUE)),log(max(test, na.rm=TRUE))))
for (i in 1:dim(test)[[2]]) {
  subset <- log(test[,i])
  diffs <- diff(subset)
  for (j in 1:(length(subset)-1)) {
    y <- c(subset[[j]],subset[[j+1]])
    lines(c(i,i),y, pch=19, cex=0.5, col=cols[[j]], lwd=3)
  }
#   lines(c(i,i), c(min(subset, na.rm=TRUE),max(subset, na.rm=TRUE)))
}



