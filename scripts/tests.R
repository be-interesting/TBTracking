c1 <- getCentroids()

groups <- findSimilarGroups(getCentroids(test2),getCentroids(test3))

dims <- dim(test2)

display(matrix(test2 %in% groups$b1, nrow=dims[[1]], ncol=dims[[2]]))
display(matrix(test3 %in% groups$b2, nrow=dims[[1]], ncol=dims[[2]]))
