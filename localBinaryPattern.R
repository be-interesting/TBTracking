frame.3 <- rotate(readImage("data/frame3.tif"),0.5)
mask <- rotate(readImage("artifactMask.tiff"),0.5)
frame.3[mask>0] <- 1

blur <- function(m) {
  
}

flo = makeBrush(3, shape='disc', step=FALSE)^2
flo = flo/sum(flo)
imgflo = filter2(homo[1:50,], flo)
