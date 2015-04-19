library(EBImage)
library(glcm)
library(plyr)
library(RColorBrewer)

source("scripts/loadFrames.R")
source("scripts/processImages.R")
source("scripts/createArtifactMask.R")
source("scripts/darkLineMask.R")
source("scripts/removeBlobs.R")
source("scripts/isolateBacteria.R")
source("scripts/getCentroids.R")
source("scripts/findSimilarGroups.R")
source("scripts/appendOutput.R")
source("scripts/updateCentroidIDs.R")

# Generate output for a given folder
# Optional: if n, only process that many images
main <- function(dataDir="examples/set_2", n) {
  
  # Load frames
  frames <- loadFrames(dataDir, n=10)
  
  # Create artifact mask
  artifactMask <- createArtifactMask(frames[[1]]@.Data)
  
  frames.labeled <- lapply(frames, isolateBacteria)
  
  i <- 0
  for (frame in frames.labeled) {
    i <- i+1
    writeImage(frame,paste0("frames1/",i,".tif"))    
    Sys.sleep(0.3)
  }
  
  i <- 0
  for (frame in frames) {
    i <- i+1
    writeImage(frame,paste0("frames2/",i,".tif"))    
    Sys.sleep(0.3)
  }
  
  
}


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




  
