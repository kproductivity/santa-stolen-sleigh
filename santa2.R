ptm <- proc.time()

library(data.table)

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


for (k in 1000:k.init){
    gifts$TripId <- kmeans(Location, centers = k, iter.max = 50)$cluster    
    tripWeight <- aggregate(Weight~TripId, data=gifts, FUN=sum)$Weight
    if (max(tripWeight) <= wlimit) break
}    

summary(tripWeight)

#Write submission file
cat("Saving file... \n")
submit <- data.frame(GiftId=gifts$GiftId, TripId=gifts$TripId)
write.csv(submit, file="submission.csv", row.names = F)

cat("Processing time:")
proc.time() - ptm
