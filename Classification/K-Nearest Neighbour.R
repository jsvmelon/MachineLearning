# K- Neareast Neighbour

# Import the dataset
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[, 3:5] # this is just an example we might need from time to time

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

# Fitting K-NN to the training set and predicting the test set results (no classifier needed for K-NN)
library(class)
y_pred = knn(train = training_set[,-3],
             test = test_set[,-3],
             cl = training_set[,3],
             5)

# making the confusion matrix
cm <- table(test_set[, 3], y_pred)

# visualise the results for the training set
library(ElemStatLearn)
set <- training_set

#
# create fake test data to fill the whole area - this illustrates the classification boundaries
# the very high resolution makes the creation of the plot slow but allows to see the boundary in high-res
#
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid <- knn(train = training_set[,-3],
              test = grid_set,
              cl = training_set[,3],
              5)

plot(set[, -3],
     main = "K-NN (Training Set)",
     xlab = "Age" , ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato")) # add the fake data to fill the background
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3")) # these are the actual predictions as points on the background

#
# visualise the results for the test set
#
library(ElemStatLearn)
set <- test_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) = c("Age","EstimatedSalary")
y_grid <- knn(train = training_set[,-3],
              test = grid_set,
              cl = training_set[,3],
              5)

plot(set[, -3],
     main = "K-NN (Test Set)",
     xlab = "Age" , ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3"))



