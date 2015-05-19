
# Add a grid overlay to an image with a given spacing. Grid labels are on 
# The top and left side.
addGridToImage <- function(image, spacing=200, startx=0, starty=0) {
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  xLines <- seq(1,dimy,spacing)
  yLines <- seq(1,dimx,spacing)
  
  image[c(yLines),] <- 1
  image[,c(xLines)] <- 1
  
  plotf <- function() {
    for (x in xLines) {
      text(20,x+15,starty+(x-1),cex=1.2)
    }
    
    for (y in yLines) {
      text(y+20,20,startx+(y-1),cex=1.2)
    }
  }
  
  labels <- plotToOverlay(plotf, dimx, dimy)
  
  image[labels > 0] <- 1
  
  return(image)
  
}

addBlobOverlaysToImage <- function(image, labels) {
  image[labels > 0] <- 1
  return(image)
}

# Add the unique ID of blobs in an image. Required an image and 
# A DF of ID and numeric labels
addBlobLabelsToImage <- function(image, centroids, labels=NA, startx=0, starty=0, 
                                 overlay=FALSE, labelbg=FALSE) {
 
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  if (overlay) {
    image[labels > 0] <- 1
  }
  
  labelf = function() {
    text(centroids$x, centroids$ymin-8, centroids$id, cex=1)
  }
  
  rectf = function() {
    rect(centroids$x-40, centroids$ymin-16, centroids$x+40, centroids$ymin, col='black')
  }
  
  if (labelbg) {
    rectmat <- plotToOverlay(rectf, dimx, dimy)
    image[rectmat > 0.5] <- image[rectmat > 0.5] * 0.6
  }
    
  labmat <- plotToOverlay(labelf, dimx, dimy)
  
  image[labmat > 0.3] <- 1

  return(image)
  
}






# Process for getting text as overlay mask
# Save as png, load, temove file
plotToOverlay = function(plotf, dimx, dimy) {
  
  # Initialize temporary png
  png("temp123423453456.png",width=dimx,height=dimy)
  
  # Save par
  oldPar <- par()
  
  # Set 0 margin parameters
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  
  # Blank plot with proper parameters
  plot(0,0, ylim=c(dimy,1), xlim=c(1,dimx), type="n", axes=T, xaxt="n", yaxt='n', xaxs='i', yaxs='i')
  
  # Execute plotting function
  plotf()
  dev.off()
  
  # Reset parameters
  par(oldPar)
  
  # Now load the png as an "image" type
  labels <- 1 - readImage("temp123423453456.png")[,,1]
  
  # Delete the file
  file.remove("temp123423453456.png")
  
  # Return the mask
  return(labels)
  
}
