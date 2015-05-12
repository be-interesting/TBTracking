# Reads the first n tif files in a given directory. Also rotates images if desired
loadFrames <- function(dir, n=NA, rotation=0) {

  read <- function(image) {
    return(rotate(readImage(image)@.Data, rotation))
  }
  
  files <- list.files(dir, pattern="\\.tif$", full.names=TRUE)
  if (!is.na(n)) {
    files <- files[1:n]
  }
  return(lapply(files, read))
  
}