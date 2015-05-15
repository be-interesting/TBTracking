



# Accepts a list of matrices and creates a gif
makeGif <- function(frames, filename, delay, resize=100) {
  
  # Temporary directory location. We store frames here
  tempDir <- "temp12342345/"
  
  dir.create(tempDir)
  
  # Save each image as a tiff frame
  for (i in 1:length(frames)) {
    j <- ifelse(i<10,paste0("0",i),i)
    writeImage(frames[[i]],paste0(tempDir,j,".tiff"))
  }
  
  # Convert frames to gif
  system(paste0('convert -resize "', resize, '%" ', tempDir, '*.tiff ',
                filename))
  
  # Delete temporary directory
  unlink(tempDir, recursive=TRUE)
  
}