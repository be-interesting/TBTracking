library(EBImage)

source("createFillCircle.R")
source("createArtifactMask.R")

# Load artifact mask TODO should this include removing dark lines?
# Align with background
# "Levels" algorithm to isolate bacteria as black pixels
# Statistically group black clumps together

# # read in the background frame for testing
frame.bg <- readImage("data/background.tif")
# 
# dark <- frame.bg@.Data < 0.32
# frame.1[dark] <- 1
# 
# 
# dark[dark==0] <- 1
# display(dark)
# 
# frame.1 <- readImage("data/frame1.tif")
# 
# 
# 
# test <- frame.1 * dark


# This definitely works
test <- createArtifactMask(frame.bg)
test <- frame.bg * test
test[test==0] <- 1

# sq <- readImage("examples/d1.tif")
# display(sq[22:237, 5:220])


#215 #215



