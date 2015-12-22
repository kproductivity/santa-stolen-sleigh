ptm <- proc.time()

library(data.table)

##Read data
cat("Reading data... \n")
gifts <- fread("../input/gifts.csv")

##Find the number of clusters that satisfy the weight limit
#Ideally, this should've been done together with the clustering phase, though
cat("Grouping clusters... \n")

#Initialise TripId
gifts$TripId <- c(1:100000)

#Set parameters
set.seed(123)
wlimit <- 1000
k <- 2000


Location <- cbind(gifts[, Longitude], gifts[, Latitude])
summary(Location)


for (i in 1000:k)
{
    gifts$TripId <- kmeans(Location, centers = k, iter.max = 30)$cluster
    if (max(aggregate(Weight~TripId, data=gifts, FUN=sum)$Weight) <= wlimit) break
}

summary(aggregate(Weight~TripId, data=gifts, FUN=sum)$Weight)

#Write submission file
cat("Saving file... \n")
submit <- data.frame(GiftId=gifts$GiftId, TripId=gifts$TripId)
write.table(submit, file="submission.csv", row.names = F)

cat("Processing time:")
proc.time() - ptm
