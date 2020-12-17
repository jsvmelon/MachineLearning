# Logistic Regression

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

# Fitting Logistic Regression to the training set
classifier <- glm(formula = Purchased ~ ., 
                  family = binomial,
                  data = training_set)

# Predicting the test set results
prob_pred <- predict(classifier, 
                     type = "response",
                     newdata = test_set[-3])

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- ifelse(prob_pred > 0.5, 1, 0) 

# making the confusion matrix
cm <- table(test_set[, 3], y_pred)

# visualise the results
library(ElemStatLearn)
set <- training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) = c("Age","EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)

library(ggplot2)
ggplot() + 
  geom_point(aes(x = grid_set$Age, y = grid_set$EstimatedSalary), colour = ifelse(y_grid == 1, "springgreen3", "tomato"),size = 0.1) +
  geom_point(aes(x = set$Age, y = set$EstimatedSalary), fill = ifelse(set[, 3] == 1, "green4", "red3"), size = 2, shape = 21) +
  xlab("Age") + ylab("Estimated Salary")

plot(set[, -3],
     main = "logistic regresssion (training set)",
     xlab = "Age" , ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3"))

# visualise the results for the test set
library(ElemStatLearn)
set <- test_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) = c("Age","EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = "logistic regresssion (Test set)",
     xlab = "Age" , ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3"))



