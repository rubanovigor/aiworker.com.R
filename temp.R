# -- Determine number of clusters
wss <- (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(x,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# -- Using hclust and cutree can also set the number of clusters
hc <- hclust(dist(x), "ward")
plot(hc) # the plot can also help to decide the # of clusters
memb <- cutree(hc, k = 2)