library(data.table)
library(geosphere)
library(cluster)

#Read gift location data
gifts <- fread("input/gifts.csv")

str(gifts)
dim(gifts)

################################
# Ward Hierarchical Clustering #
################################

#d <- dist(gifts[,Latitude, Longitude], method = "euclidean") # distance matrix

# Use Haversine distance 
myGift <- as.matrix(gifts[,Latitude,Longitude])
t.myGift <- t(myGift)
myDist <- as.dist(apply(myGift, 1, function(x) distHaversine(c(x[2],x[1]),
                                                             c(t.myGift[2,],t.myGift[1,]))))

fit <- hclust(myDist, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")



