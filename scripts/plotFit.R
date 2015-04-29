f1 <- function(x,y) { return( (0.5^(x/10)) * (y^0.5) ) }

pdf("test.pdf")

plot(0,0,xlim=c(0,25),ylim=c(0,1),xlab="distance (pixels)", ylab="size ratio",type="n", main="0.5^(x/10) * y^0.5")
for (x in seq(0,25,0.5)) {
  for (y in seq(0,1,0.022)) {
    z <- f1(x,y)^0.75
    points(x,y,pch=15,cex=1.5, col=rgb(z,z,z))
  }
}

dev.off()