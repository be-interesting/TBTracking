

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
