
# Updates a centroid dataframe with IDs that are determined to be
# the same from the previous frame
updateCentroidIDs <- function(c1, g) {
  
  # select columns that continue between frame1 & 2
  idList <- as.character(c1$id)
  id1 <- as.character(g$id1)
  id2 <- as.character(g$id2)
  c1$id <- as.character(lapply(idList, function(x) if(x %in% id2) id1[which(id2==x)] else x))
  
  return(c1)
}