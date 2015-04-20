# Aligns, and recolors all of the .tif images in a folder
# Assumes that the first frame is a background image
# Arguments: list of images to be aligned, sample space to align to (x,y,side length)
processImages <- function(images, sample=c(150,250,150)) {
  
    normalizeValue <- function(m) {      
      # brighten
      m <- m / max(m)
      m <- rotate(m, 0.5)
      return(m)
    }
  
    x1 <- sample[[1]]
    y1 <- sample[[2]]
    wh <- sample[[3]]
  
    # Adjust image brightness
    images <- lapply(images, normalizeValue)
    
    # Get background region
    bgSample <- images[[1]][x1:(x1+wh), y1:(y1+wh)]
    
    
    
    for (i in 2:length(images)) {
      
      # Debug
      print(paste0("Aligning frame ", i))
      
      image <- images[[i]]@.Data
      
      # Take a sample 50px larger on each side than the background sample
      subset <- image[(x1-50):(x1+wh+50), (y1-50):(y1+wh+50)]
      
      # Initialize matrix for recording differences
      diffs <- matrix(NA,nrow=100,ncol=100)
      
      # Search the subset space for the least different region
      for (x in 1:100) {
        for (y in 1:100) {
          sample <- subset[x:(x+wh),y:(y+wh)]
          diffs[x,y] <- mean(abs(sample-bgSample))
        }
      }
      
      # Find the x and y offsets
      offset.x <- which(diffs == min(diffs),arr.ind=T)[[1]] - 50
      offset.y <- which(diffs == min(diffs),arr.ind=T)[[2]] - 50
      
      # Image dimensions
      width <- dim(image)[[1]]
      height <- dim(image)[[2]]
      
      # Crop image, adjust by offsets
      images[[i]] <- image[(150 + offset.x):(width + offset.x - 150),
                           (150 + offset.y):(height + offset.y - 150)]
      
    }
    
    background <- images[[1]]
    
    # Crop background
    background <- background[150:(dim(background)[[1]]-150),
                             150:(dim(background)[[2]]-150)]
    
    images[[1]] <- background
    
    return(images)
    

  
  
}



# 
# images <- alignImages(images)
# lapply(images, display)
# 
# 
# ### TEMPORARY 
# ### Subset and save these images
# 
# saveSmallSubset <- function(image, filename) {
#   writeImage(image[1020:1620,480:1080], paste0("examples/set_2/", filename))
# }
# 
# lapply(images, saveSmallSubset)
# 
# for (i in 1:length(images)) {
#   im <- images[[i]][1060:2200,455:1104]
#   writeImage(im, paste0("examples/set_2/", LETTERS[i], ".tif"))
# }