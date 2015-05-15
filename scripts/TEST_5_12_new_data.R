# Work computer
dataDir <- "/Volumes/KINGSTON/Shearman_lab_Rif-INH Drug Tolerance, 5-14-14, Chamber C/xy6//Phase"
# Laptop
dataDir <- "~/Desktop/tbTest/xy6/Phase/"

# Load 10 frames into a list
frames <- loadFrames(dataDir, 10, -1.5)

# Brighten and align frames
frames.processed <- processImages(frames, sample=c(300,100,100), crop=10)

# Make artifact mask
artifactMask <- createArtifactMask(frames[[1]]@.Data)
