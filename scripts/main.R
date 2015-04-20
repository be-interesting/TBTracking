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
  frames <- loadFrames(dataDir, n=20)
  
  # Create artifact mask
  artifactMask <- createArtifactMask(frames[[1]]@.Data)
  
  frames.labeled <- lapply(frames, isolateBacteria)
  
  
  
  firstFrame <- frames.labeled[[2]]
  centroidsBefore <- getCentroids(firstFrame)
  
  output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))
  
  saved <- vector("list", length(frames.labeled))
  saved[[2]] <- centroidsBefore
  
  for (i in 3:length(frames.labeled)) {
    
    
    frame <- frames.labeled[[i]]
    
    print(paste0("Processing frame ", i, " of ", length(frames)))
    
    centroidsAfter <- getCentroids(frame)
    
    
    # Find groups that are determined to be the same between the two frames
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter)
    
    # For those continued group, give them the proper ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    output <- appendOutput(centroidsAfter, groups, output)
    
    saved[[i]] <- centroidsAfter
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
    
  }
  
  
  # A neat plot
  save <- output
  output <- output[,apply(output, 2, function(x) sum(!is.na(x)) > 8)]
  
  plot(log(output[,1]), log="y", type="n", ylim=c(log(min(output,na.rm=TRUE)),log(max(output, na.rm=TRUE))), xlim=c(1,20),
       xlab="timestep", ylab="log(size)")
  lapply(log(output), lines, lwd=2, col=rgb(0,0,0,0.3))
  lapply(log(output), points, col=rgb(0,0,0,0.4), cex=0.4, pch=19)
  
}

#   i <- 0
#   for (frame in frames.labeled) {
#     i <- i+1
#     writeImage(frame,paste0("frames1/",i,".tif"))    
#     Sys.sleep(0.3)
#   }
#   
#   i <- 0
#   for (frame in frames) {
#     i <- i+1
#     writeImage(frame,paste0("frames2/",i,".tif"))    
#     Sys.sleep(0.3)
#   }

boxplot(t(log(output)), xlab = "timestep", ylab = "log( size )",)

# For a given id, "display" each frame that contains that id
showChanges <- function(id, frames, centroids) {
  for (i in 2:length(frames)) {
    if (sum(centroids[[i]]$id == id) > 0) {
      label <- which(centroids[[i]]$id == id)
      display(frames[[i]] == label)
      Sys.sleep(0.3)
    }
  }
}

showChanges(colnames(output)[[1]], frames.labeled, saved)

# Look at each line individually
for (i in 1:dim(output)[[2]]) {
  print(i)
  plot(log(output[,i]), log="y", type="l", ylim=c(log(min(output,na.rm=TRUE)),log(max(output, na.rm=TRUE))),
       xlim=c(0,25),xlab="timestep", ylab="log(size)")
  Sys.sleep(0.5)
}  
