loadFrames <- function(dir, n=NA) {

  read <- function(image) {
    return(readImage(image)@.Data)
  }
  
  files <- list.files(dir, pattern="\\.tif$", full.names=TRUE)
  if (!is.na(n)) {
    files <- files[1:n]
  }
  return(lapply(files, read))
  
}