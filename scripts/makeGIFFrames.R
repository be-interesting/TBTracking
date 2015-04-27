showChanges <- function(id, f, centroids) {
  dir.create(paste0("images/",id))
  for (i in 2:length(frames)) {
    if (sum(centroids[[i]]$id == id) > 0) {
      label <- which(centroids[[i]]$id == id)
      label <- centroids[[i]]$index[[label]]
      mask <- f[[i]] == label
      f1 <- frames[[i]]
      f1[mask] <- 1
      j <- ifelse(i<10,paste0("0",i),i)
      writeImage(f1,paste0("images/",id,"/",j,".tif"))
      Sys.sleep(0.3)
    } else {
      j <- ifelse(i<10,paste0("0",i),i)
      writeImage(frames[[i]],paste0("images/",id,"/",j,".tif"))
    }
  }
}

for (i in 26:50) {
  showChanges(colnames(output)[[i]], frames.labeled, saved)
}

