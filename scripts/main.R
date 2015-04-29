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
main <- function(dataDir="images/full_post_cropped", n) {
  
  # Load frames
  ptm <- proc.time()
  frames <- loadFrames(dataDir, n=35)
  print(proc.time() - ptm)
  
  # Create artifact mask
  ptm <- proc.time()
  artifactMask <- createArtifactMask(frames[[1]]@.Data)
  print(proc.time() - ptm)
  
  frames.labeled <- lapply(frames, isolateBacteria)

  firstFrame <- frames.labeled[[2]]
  centroidsBefore <- getCentroids(firstFrame)
  centroidsBefore <- centroidsBefore[!(round(centroidsBefore$y) %in% c(seq(690,747),seq(1586,1654))),]
  
  output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))
  
  saved <- vector("list", length(frames.labeled))
  saved[[2]] <- centroidsBefore
  
  for (i in 3:length(frames.labeled)) {
    
    ptm <- proc.time()
    
    frame <- frames.labeled[[i]]
    
    print(paste0("Processing frame ", i, " of ", length(frames)))
    
    centroidsAfter <- getCentroids(frame)
    centroidsAfter <- centroidsAfter[!(round(centroidsAfter$y) %in% c(seq(690,747),seq(1586,1654))),]
    
    # Find groups that are determined to be the same between the two frames
    groups <- findSimilarGroups(centroidsBefore,centroidsAfter)
    
    # For those continued group, give them the proper ID's from the previous frame
    centroidsAfter <- updateCentroidIDs(centroidsAfter, groups)
    output <- appendOutput(centroidsAfter, output)
    
    saved[[i]] <- centroidsAfter
    
    # Reassign "before" centroids to the current frame
    centroidsBefore <- centroidsAfter
    
    print(proc.time() - ptm)
    
  }
  
  
  # A neat plot
  save <- output
  output <- output[,apply(output, 2, function(x) sum(!is.na(x)) > 15)]
  
  # Log
  plot(output[,1], log="y", type="n", ylim=c(min(output,na.rm=TRUE),max(output, na.rm=TRUE)),
       xlab="timestep", ylab="log(size)")
  lapply(output, lines, lwd=2, col=rgb(0,0,0,0.3))
  lapply(output, points, col=rgb(0,0,0,0.4), cex=0.4, pch=19)

  
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

boxplot(t(output), log="y", xlab = "timestep", ylab = "log( size )",)

# For a given id, "display" each frame that contains that id
showChanges <- function(id, f, centroids) {
  for (i in 2:length(f)) {
    if (sum(centroids[[i]]$id == id) > 0) {
      label <- which(centroids[[i]]$id == id)
      label <- centroids[[i]]$index[[label]]
#       display(f[[i]]==label)
      mask <- f[[i]] == label
      f1 <- frames[[i]]
      f1[mask] <- 1
      display(f1)
      Sys.sleep(0.3)
    }
  }
}

showChanges(colnames(output)[[2]], frames.labeled, saved)

# Look at each line individually
for (i in 1:dim(output)[[2]]) {
  print(i)
  plot(log(output[,i]), log="y", type="l", ylim=c(log(min(output,na.rm=TRUE)),log(max(output, na.rm=TRUE))),
       xlim=c(0,25),xlab="timestep", ylab="log(size)")
  Sys.sleep(0.5)
}  
