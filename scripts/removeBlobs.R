# Remove all blobs under a certain size threshold
removeBlobs <- function(m, size) {
  # Label blobs
  m <- bwlabel(m)
  # Save dimensions
  dims <- dim(m)
  # Unravel matrix and count the occurances of each label
  sorted <- sort(table(as.numeric(m)))
  # Labels which are under the threshold
  small <- as.numeric(names(sorted[sorted<size]))
  # Remove pixels that fall into undersized labels
  m[m %in% small] <- 0
  # Turn back into matrix
  m <- matrix(m, nrow=dims[[1]], ncol=dims[[2]])
  return(m)
}