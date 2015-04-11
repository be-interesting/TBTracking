library(EBImage)
library(glcm)

source("scripts/createArtifactMask.R")
source("scripts/darkLineMask.R")
source("scripts/removeBlobs.R")
source("scripts/isolateBacteria.R")
source("scripts/getCentroids.R")
source("scripts/findSimilarGroups.R")


# read in the background frame
frame01 <- rotate(readImage("examples/set_3/frame01.tif"),0.5)

# artifactMask <- createArtifactMask(frame01@.Data)
# artifactMask <- artifactMask < 1 # invert

# Save it
writeImage(artifactMask, "examples/artifactMask.tiff")
artifactMask <- readImage("examples/artifactMask.tiff")

# Load sample data
frame02 <- rotate(readImage("examples/set_3/frame02.tif"),0.5)
frame03 <- rotate(readImage("examples/set_3/frame03.tif"),0.5)
frame04 <- rotate(readImage("examples/set_3/frame04.tif"),0.5)
frame05 <- rotate(readImage("examples/set_3/frame05.tif"),0.5)
frame06 <- rotate(readImage("examples/set_3/frame06.tif"),0.5)
frame07 <- rotate(readImage("examples/set_3/frame07.tif"),0.5)
frame08 <- rotate(readImage("examples/set_3/frame08.tif"),0.5)
frame09 <- rotate(readImage("examples/set_3/frame09.tif"),0.5)
frame10 <- rotate(readImage("examples/set_3/frame10.tif"),0.5)
frame11 <- rotate(readImage("examples/set_3/frame11.tif"),0.5)




test2 <- isolateBacteria(frame02@.Data)
test3 <- isolateBacteria(frame03@.Data)
test4 <- isolateBacteria(frame04@.Data)
test5 <- isolateBacteria(frame05@.Data)
test6 <- isolateBacteria(frame06@.Data)
test7 <- isolateBacteria(frame07@.Data)
test8 <- isolateBacteria(frame08@.Data)
test9 <- isolateBacteria(frame09@.Data)
test10 <- isolateBacteria(frame10@.Data)
test11 <- isolateBacteria(frame11@.Data)



