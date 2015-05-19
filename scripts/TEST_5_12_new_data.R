# Work computer
dataDir <- "~/Desktop//TBData//xy6/Phase/"
# Laptop
dataDir <- "~/Desktop/tbTest/xy6/Phase/"

# Load 10 frames into a list
frames <- loadFrames(dataDir, 10, -1.5)

# Brighten and align frames
frames <- processImages(frames, sample=c(300,100,100), crop=35)

# Make artifact mask
artifactMask <- createArtifactMask(frames[[1]]@.Data)
# Just for this
artifactMask[0:75,422:650] <- 1
artifactMask[,c(0:150, 900:971)] <- 1

# Labels
frames.labeled <- lapply(frames, isolateBacteria)




firstFrame <- frames.labeled[[2]]
centroidsBefore <- getCentroids(firstFrame)

output <- data.frame(t(data.frame(centroidsBefore$size,row.names=centroidsBefore$id)))

saved <- vector("list", length(frames.labeled))
saved[[2]] <- centroidsBefore

for (i in 3:length(frames.labeled)) {
  
  ptm <- proc.time()
  
  frame <- frames.labeled[[i]]
  
  print(paste0("Processing frame ", i, " of ", length(frames)))
  
  centroidsAfter <- getCentroids(frame)
  
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


saved[[1]] <- NULL
frames[[1]] <- NULL
frames.labeled[[1]] <- NULL

overlays <- mapply(addBlobOverlaysToImage, frames, frames.labeled, SIMPLIFY=FALSE)
overlays <- mapply(addGridToImage, overlays, SIMPLIFY=FALSE)
noOverlays <- mapply(addBlobLabelsToImage, frames, saved, frames.labeled, SIMPLIFY=FALSE)
noOverlays <- mapply(addGridToImage, noOverlays, SIMPLIFY=FALSE)

stitched <- mapply(stitchImages, noOverlays, overlays, SIMPLIFY=FALSE)

makeIndividualGifs(ids = colnames(output), frames = frames, labels = frames.labeled, centroids = saved)
