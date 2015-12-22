ptm <- proc.time()

library(data.table)
library(geosphere)

#Read data
cat("Reading data... \n")
gifts <- fread("input/gifts.csv")

#Find the number of clusters that satisfy the weight limit
#Ideally, this should've been done together with the clustering phase, though
cat("Grouping clusters... \n")

set.seed(123)
k.init <- 5000
wlimit <- 1000

Location <- cbind(gifts[, Longitude], gifts[, Latitude])
summary(Location)

first <- kmeans(Location, centers = k.init, iter.max = 50)    
gifts$cluster <- first$cluster

#First clustering results
summary(aggregate(Weight~TripId, data=gifts, sum)$Weight)


#Then, hierarchical clustering on the clustered centers
ByLocation <- cbind(first$centers[,1], first$centers[,2])

d <- as.dist(apply(ByLocation, 1,
                   function(x) distHaversine(x, ByLocation)))

second <- hclust(d)

for (i in 1000:k.init){
    newClusters <- data.frame(TripId=c(1:k.init), cluster=cutree(second, k=i))
    evalClusters <- merge(gifts, newClusters, by="TripId")
    if (max(aggregate(Weight~cluster, data=evalClusters, FUN=sum)$Weight) <= wlimit) break
}    


#Write submission file
cat("Saving file... \n")
submit <- data.frame(GiftId=gifts$GiftId, TripId=gifts$cluster)
write.csv(submit, file="submission.csv", row.names = F)

cat("Processing time:")
proc.time() - ptm
