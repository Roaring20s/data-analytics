dar2ed.kmeans.plot <- function (data, cols, num_clust = 10, seed = 9876)  {
  n = length(names(data))
  dat = dar2ed.scale.many(data, cols)[, (n + 1):(n + length(cols))]
  # dat.scale = scale(dat)
  wss <- (nrow(dat) - 1) * sum(apply(dat, 2, var))
  for (i in 2:num_clust) {
    set.seed(seed)
    wss[i] <- sum(kmeans(dat, centers = i)$withinss)
  }
  plot(1:num_clust, wss, type = "b", pch = 5, xlab = "# Clusters", 
       ylab = "Total within_ss across clusters")
}



