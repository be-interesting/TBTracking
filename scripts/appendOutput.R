
# update the output with a new timestep and 
appendOutput <- function(c1, g, output) {
  
  # find 
  newIDs <- c1[!(c1$id %in% colnames(output)),]
  
  # Create empty dataframe for new IDs
  newIDs.df <- data.frame(t(data.frame(rep(NA,dim(newIDs)[[1]]))))
  colnames(newIDs.df) <- newIDs$id
  
  # Bind current output with new IDs
  allIDs.df <- cbind(output, newIDs.df)
  
  # Create a new row with all of the new values
  newRow <- data.frame(t(data.frame(c1$size, row.names=c1$id)))
  
  # Add the new row with rbind.fill, which replaces missing values with NA
  return(rbind.fill(output, newRow))
  
}


