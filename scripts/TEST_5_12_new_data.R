# Work computer
dataDir <- "/Volumes/KINGSTON/Shearman_lab_Rif-INH Drug Tolerance, 5-14-14, Chamber C/xy6//Phase"
# Laptop
dataDir <- "~/Desktop/tbTest/xy6/Phase/"

# Load 10 frames into a list
frames <- loadFrames(dataDir, 40, -1.5)

# Brighten and align frames
frames <- processImages(frames, sample=c(300,100,100), crop=35)

# Make artifact mask
artifactMask <- createArtifactMask(frames[[1]]@.Data)
# Just for this
artifactMask[0:75,465:681] <- 1
artifactMask[,c(0:173, 945:1019)] <- 1

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
