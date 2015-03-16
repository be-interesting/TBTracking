library(EBImage)

source("createFillCircle.R")
source("createArtifactMask.R")

# Load artifact mask TODO should this include removing dark lines?
# Align with background
# "Levels" algorithm to isolate bacteria as black pixels
# Statistically group black clumps together

# read in the background frame for testing
frame.bg <- readImage("data/background.tif")

# This definitely works
test <- createArtifactMask(bg)
test <- frame.bg * test
test[test==0] <- 1







