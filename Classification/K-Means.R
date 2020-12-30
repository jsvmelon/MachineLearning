# Classification

# Import the dataset
dataset <- read.csv("Mall_Customers.csv")
X <- dataset[, 4:5] # this is just an example we might need from time to time

# using the elbbow method to find the optimal number of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X, i)$withinss)
plot(1:10, wcss, type = "b", main = "Cluster of Clients", xlab = "Number of clusters", ylab = "WCSS")

# fit using k-means
set.seed(29)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

# visualisee teh clusters
library(cluster)
clusplot(X, 
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster of Clients",
         xlab = "Annual Income",
         ylab = "Spending Scor")
