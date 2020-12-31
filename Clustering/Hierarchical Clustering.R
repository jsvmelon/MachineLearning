# Clustering

# Import the dataset
dataset <- read.csv("Mall_Customers.csv")
X <- dataset[,4:5]

# build dendogram to find the optimal number of clusters
dendogram <- hclust(dist(X, method = "euclidean"), method = "ward.D")

plot(dendogram,
     main = "Dendrogram",
     xlab = "Customers",
     ylab = "Euclidean distance")

# fitting HC clustering to the dataset with 5 clusters
y_hc <- cutree(dendogram,5)

# display the results
library(ggplot2)
ggplot() + 
  theme_light() + 
  geom_point(aes(x = X$Annual.Income..k.., y = X$Spending.Score..1.100., color = as.factor(y_hc)), shape = y_hc) +
  stat_ellipse(aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = as.factor(y_hc)),data = X, geom = "path") +
  ylab("Spending Score") + xlab("Annual Income") + labs(color = "Cluster")
  
# this is the visualisation used in the course
# however, the projection used by clusplot makes it hard to read the chart as (in this case) the y axis is inverted and the axis values are not showing the data
library(cluster)
clusplot(X, 
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster of Clients",
         xlab = "Annual Income",
         ylab = "Spending Scor")
