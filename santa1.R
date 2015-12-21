ptm <- proc.time()

library(data.table)
library(geosphere)
library(fastcluster) #See http://www.jstatsoft.org/article/view/v053i09
library(ddR)
library(kmeans.ddR)

#Read data
cat("Reading data... \n")
gifts <- fread("input/gifts.csv")

#Calculate first tier clusters
#as suggested by William Cukierski (TY!)
Location <- cbind(gifts[, Longitude], gifts[, Latitude])
summary(Location)

dgifts <- as.darray(Location)
k <- 5000 #to later tune
first <- dkmeans(dgifts, centers = k)

#Allocate new centers
#newCenters <- data.frame(cluster1=c(1:k), first$centers)
names(newCenters)[2:3]<-c("Longitude", "Latitude")
firstClusters <- data.frame(GiftId=c(1:100000),
                            Weight=gifts$Weight,
                            cluster1=predict(first, Location))
#firstClusters <- merge(firstClusters, newCenters, by="cluster")

#Calculate distances
cat("Calculating Haversine distances... \n")

ByLocation <- cbind(first$centers[,1], first$centers[,2])
d <- as.dist(apply(ByLocation, 1,
                   function(x) distHaversine(x, ByLocation)))

#Hierarchical cluster analysis
cat("Calculating clusters... \n")
hc <- hclust(d, method = "centroid")


#Find the number of clusters that satisfy the weight limit
#Ideally, this should've been done together with the clustering phase, though
cat("Grouping clusters... \n")
wlimit <- 1000

for (i in 1:k)
{
    newClusters <- data.frame(cluster1=c(1:k), cluster2=cutree(hc, k=i))
    evalClusters <- merge(firstClusters, newClusters, by="cluster1")
    if (max(aggregate(Weight~cluster2, data=evalClusters, FUN=sum)) <= wlimit) break
}

#Write submission file
submit <- data.frame(GiftId=evalClusters$GiftId, TripId=evalClusters$cluster2)
write.table(submit, file="submission.csv", row.names = F)

cat("Processing time:")
proc.time() - ptm
