


# Accepts a list of matrices and creates a gif
makeGif <- function(frames, filename, delay=15, resize=80) {
  
  # Temporary directory location. We store frames here
  tempDir <- "temp12342345/"
  
  dir.create(tempDir)
  
  # Save each image as a tiff frame
  for (i in 1:length(frames)) {
    j <- ifelse(i<10,paste0("0",i),i)
    writeImage(frames[[i]],paste0(tempDir,j,".tiff"))
  }
  
  # Convert frames to gif
  system(paste0('convert -resize "', resize, '%" -delay  ', delay, ' ', tempDir, '*.tiff ',
                filename))
  
  # Delete temporary directory
  unlink(tempDir, recursive=TRUE)
  
}


# For each of the given ids and output, create an individual gif
makeIndividualGifs <- function(ids,frames,labels,centroids,outputDir="individualGifs") {
  
  dir.create(outputDir, showWarnings=FALSE)
  
  for (id in ids) {
    subsets <- makeIndividualGif(id, frames, labels, centroids)
    makeGif(subsets, paste0(outputDir, "/", id,".gif"))
  }
  
}


# Returns a mask of the given blob id through time cropped around the blob
makeIndividualGif <- function(id, frames, labels, centroids) {
  
  # Initialize return list
  ret <- vector("list", length(frames))
  
  # Extract blob origin coordinates from id name
  coords <- as.numeric(unlist(regmatches(id, gregexpr('\\(?[0-9,.]+', id))))[1:2]
  c1 <- coords - 100
  c2 <- coords + 100
  
  
  # Ensure coordinates are within bounds
  if (c1[[1]] < 0) {
    c2[[1]] <- c2[[1]] + abs(c1[[1]])
    c1[[1]] <- c1[[1]] + abs(c1[[1]])
  }
  if (c1[[2]] < 0) {
    c2[[2]] <- c2[[2]] + abs(c1[[1]])
    c1[[2]] <- c1[[2]] + abs(c1[[1]])
  }
  if (c2[[1]] > dim(frames[[1]])[[1]]) {
    c1[[1]] <- c1[[1]] - (dim(frames[[1]])[[1]] - abs(c2[[1]]))
    c2[[1]] <- c2[[1]] - (dim(frames[[1]])[[1]] - abs(c2[[1]]))
  }
  if (c2[[2]] > dim(frames[[1]])[[2]]) {
    c1[[2]] <- c1[[2]] + (dim(frames[[1]])[[2]] - abs(c2[[2]]))
    c2[[2]] <- c2[[2]] + (dim(frames[[1]])[[2]] - abs(c2[[2]]))
  }
  
  
  for (i in 1:length(frames)) {
    
    # Current frame
    f1 <- frames[[i]]
    # Get instances where blobs match id
    ided <- centroids[[i]]$id == id
    # If there are any of that id in the current frame, mask it
    if (sum(ided) > 0) {
      # Index where id is found
      label <- which(ided)
      # Original blob label index
      label <- centroids[[i]]$index[[label]]
      mask <- labels[[i]] == label
      f1[mask] <- 1
    }
    
    # Crop
    f1 <- f1[c1[[1]]:c2[[1]], c1[[2]]:c2[[2]]]
    ret[[i]] <- f1
    
  }
  
  return(ret)
  
}







makeIndividualGifs(colnames(output), frames, frames.labeled, saved)
