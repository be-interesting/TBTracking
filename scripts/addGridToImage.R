addGridToImage <- function(image, spacing=200, startx=0, starty=0) {
  
  dimx <- dim(image)[[1]]
  dimy <- dim(image)[[2]]
  
  xLines <- seq(1,dimy,spacing)
  yLines <- seq(1,dimx,spacing)
  
  image[c(yLines),] <- 1
  image[,c(xLines)] <- 1
  
  png("temp123423453456.png",width=dimx,height=dimy)
  
  par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  
  plot(0,0, ylim=c(dimy,1), xlim=c(1,dimx), type="n", axes=T, xaxt="n", yaxt='n', xaxs='i', yaxs='i')
  
  for (x in xLines) {
    text(20,x+15,starty+(x-1),cex=1.2)
  }

  for (y in yLines) {
    text(y+20,20,startx+(y-1),cex=1.2)
  }
  
  dev.off()
  
  labels <- 1- readImage("temp123423453456.png")[,,1]
  
  file.remove("temp123423453456.png")
  
  image[labels > 0] <- 1
  
  return(image)
  
  
}
