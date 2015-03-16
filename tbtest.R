library(EBImage)

# read in the background frame for testing
frame <- readImage("~/Desktop/tbTest/frame1.tif")
frameCopy <- frame
frame.data <- frame@.Data

# Crank up the light
# img <- img * 18

createFillCircle <- function() {
  r <- 14
  x1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(15 + (r * cos(a)))))
  y1 <- unlist(lapply(seq(0, 2*pi, 0.05), function(a) round(15 + (r * sin(a)))))
  points <- data.frame(x1, y1)
  circle <- matrix(0, 31, 31)
  for (x in 1:31) {
    # Y values with the current x value
    yRange <- points[points$x1==x,]$y1
    if(length(yRange) < 1) {
      circle[x,] <- 1
    } else {
      yRange <- range(yRange)
      circle[x,1:yRange[[1]]] <- 1
      circle[x,yRange[[2]]:31] <- 1
    }
  }
  return(circle)
}

# --------------------------------------------- #
# REMOVING THE SMALL BLACK NODES TRY 1
# --------------------------------------------- #

# Get example files that begin with "n" (for "node")
nodeExamples <- list.files("~/Desktop/tbTest/examples/")
nodeExamples <- nodeExamples[ unlist(lapply(nodeExamples, function(x) substr(x,1,1)=="n")) ]

# Load files as image objects
nodeExamples <- lapply(nodeExamples, function(x) readImage(paste0("~/Desktop/tbTest/examples/", x))@.Data)

# Isolate the y ranges to search to speed things up
highs <- c(121, 215, 303, 393, 487, 575, 667, 755, 849, 935, 1025, 1112, 1205, 1295, 1385, 1475, 1565)

# Add 20 indices in between each element of highs to search
yIndices <- unlist(lapply(highs, function(x) seq(x, x+20)))

difs <- c()

# Loop through the y indices
for (y in yIndices) {
  
  # For each y value, search through all of the x values and find similar areas
  for (x in seq(3,(dim(frame)[[1]]-31),4)) {
  
    # Get subsets of original
    subset1 <- frame.data[x:(x+30),y:(y+30)]
#     subset2 <- frame.data[(x-2):(x+28),(y-2):(y+28)]
#     subset3 <- frame.data[(x):(x+30),(y-2):(y+28)]
#     subset4 <- frame.data[(x-2):(x+28),(y):(y+30)]

#     for (example in nodeExamples) {
#       for (subset in list(subset1, subset2, subset3, subset4)) {
      for (subset in list(subset1)) {
          
        dx.beg <- mean(abs(diff(subset[15,0:10])))
        dx.mid <- mean(abs(diff(subset[15,12:18])))
        dx.end <- mean(abs(diff(subset[15,20:30])))
        
        dy.beg <- mean(abs(diff(subset[15,0:10])))
        dy.mid <- mean(abs(diff(subset[15,12:18])))
        dy.end <- mean(abs(diff(subset[15,20:30])))
        
#         if (dx.beg > dx.mid & dx.end > dx.mid & dy.beg > dy.mid & dy.end > dy.mid) {
        if(dx.mid < 0.039 & dy.mid < 0.039 & dx.beg - dx.end < 0.039 & dy.beg - dy.end < 0.5 & dx.beg > 0.039
           & dx.end > 0.039 & dy.beg > 0.039 & dx.end > 0.039) {
           subset <- frameCopy[x:(x+30),y:(y+30)] * createFillCircle()
           subset[subset==0] <- 1
           frameCopy[x:(x+30),y:(y+30)] <- subset
        }
        
#         dif2 <- norm(abs(subset - example))
#         if (dif2 < 3.4) {
#           frame.data[x:(x+30),y:(y+30)] <- matrix(1, 31, 31)
#         }
      }
    }

  }
}

# frameCopy@.Data <- frame.data
display(frameCopy)









# --------------------------------------------- #
# REMOVING THE SMALL BLACK NODES TRY 2
# --------------------------------------------- #

# find edges
fhi <- matrix(1, nc=3, nr=3)
fhi[2,2] <- -8
edges <- filter2(frame, fhi)

# subset for testing
# edges <- edges[50:250, 125:260]

edges[edges<0.2] = 0
edges[edges>0.2] = 1

display(edges)

detected <- matrix(0, dim(edges)[[1]], dim(edges)[[2]])
i=0
for (y in yIndices) {
  i = i+1
  if (i%%10 == 0) print(paste0("another 10 rows", i))
  
  for (x in seq(1,(dim(edges)[[1]]),2)) {
    isWhite <- edges[x,y] > 0
    r <- 9 # radius in pixels
    
    if(isWhite) {
      for (i in seq(0,2*pi,0.5)) {
        x1 <- round(x + (r * cos(i)))
        y1 <- round(y + (r * sin(i)))
        if (x1>0 & y1>0 & x1<dim(edges)[[1]] & y1<dim(edges)[[2]]) {
          detected[x1, y1] <- detected[x1, y1] + 1
        }
      }
      # make find circle points
    }
  }
}

detected <- detected / max(detected)

display(detected)




# Looking at nodeExamples

plot(0,0,ylim=c(-0.5,0.5), xlim=c(0,30))
lapply(nodeExamples, function(x) lines(x[15,]))
test1 = lapply(nodeExamples, function(x) mean(abs(diff(x[15,12:18]))))
test2 = lapply(nodeExamples, function(x) mean(abs(diff(x[15,20:30]))))
a <- lapply(nodeExamples, function(x) diff(x[15,]))
test <- data.frame(a)
b <- unlist(apply(test, 1, mean))
display(nodeExamples[[1]])

