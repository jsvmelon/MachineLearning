# Random Forest Classification

# Import the dataset
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[, 3:5] # this is just an example we might need from time to time

# encode the target feature as factor
dataset$Purchased <- as.factor(dataset$Purchased)

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

# Fitting Random Forest to the training set
library(randomForest)
classifier <- randomForest(x = training_set[-3],
                           y = training_set$Purchased,
                           ntree = 500)

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- predict(classifier, newdata = test_set[-3])

# making the confusion matrix
cm <- table(test_set[, 3], y_pred)

# visualise the results for the training set
display(training_set,"Random Forest Classification (Training Set")
display(test_set,"Random Forest Classification (Test Set")

display <- function(set,title) {
        library(ElemStatLearn)
        X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
        X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
        grid_set <- expand.grid(X1,X2)
        colnames(grid_set) = c("Age","EstimatedSalary")
        y_grid <- predict(classifier, newdata = grid_set)
        
        plot(set[, -3],
             main = title,
             xlab = "Age" , ylab = "Estimated Salary",
             xlim = range(X1), ylim = range(X2))
        
        contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
        points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato"))
        points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3"))
}



