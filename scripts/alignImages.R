frame01 <- rotate(readImage("examples/full/frame01.tif"),0.5)
frame02 <- rotate(readImage("examples/full/frame02.tif"),0.5)
frame03 <- rotate(readImage("examples/full/frame03.tif"),0.5)
frame04 <- rotate(readImage("examples/full/frame04.tif"),0.5)

images <- list(frame01, frame02, frame03, frame04)


alignImages <- function(images) {
  
  for(i in 1:length(images)) {
    images[[i]] <- images[[i]]@.Data * 1/max(images[[i]])
  }
  
  background <- images[[1]]
  
  x1 <- 1800
  y1 <- 250
  wh <- 300
  
  bgSample <- background[x1:(x1+wh), y1:(y1+wh)]
  
  for (i in 2:length(images)) {
    
    image <- images[[i]]@.Data
  
    subset <- image[(x1-50):(x1+wh+50), (y1-50):(y1+wh+50)]
    
    diffs <- matrix(NA,nrow=100,ncol=100)
    
    for (x in 1:100) {
      for (y in 1:100) {
        sample <- subset[x:(x+wh),y:(y+wh)]
        diffs[x,y] <- mean(abs(sample-bgSample))
      }
    }
  
    offset.x <- which(diffs == min(diffs),arr.ind=T)[[1]] - 50
    offset.y <- which(diffs == min(diffs),arr.ind=T)[[2]] - 50
    width <- dim(image)[[1]]
    height <- dim(image)[[2]]
    
    images[[i]] <- image[(150 + offset.x):(width + offset.x - 150),
                               (150 + offset.y):(height + offset.y - 150)]
    
  }
  
  background <- background[150:(dim(background)[[1]]-150),
                           150:(dim(background)[[2]]-150)]
  
}