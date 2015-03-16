# Input: x and y dimension of matrix, circle radius
# Returns a x by y matrix with a circle of radius r at the center
# Values inside of the circle are 0, outside are 1
createFillCircle <- function(dim.x, dim.y, r) {

  # Get x and y points of circle
  x1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dim.x/2 + (r * cos(a)))))
  y1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(dim.y/2 + (r * sin(a)))))
  
  # Put points in dataframe
  points <- data.frame(x1, y1)
  
  # Initialize empty circle
  circle <- matrix(0, dim.x, dim.x)
  
  for (x in 1:dim.x) {
    # Y values with the current x value
    yRange <- points[points$x1==x,]$y1
    if(length(yRange) < 1) {
      # If there are no y values, entire row is 1
      circle[x,] <- 1
    } else {
      # If there are, fill area outside of that range with 1s
      yRange <- range(yRange)
      circle[x,1:yRange[[1]]] <- 1
      circle[x,yRange[[2]]:dim.y] <- 1
    }
  }
  
  return(circle)
  
}