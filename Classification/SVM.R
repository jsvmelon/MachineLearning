# SVM Classification

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

# Fitting XY to the training set
library(e1071)
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "linear")

classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "polynomial",
                  degree = 4)

classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "radial")

classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "sigmoid", gamma = 0.2,coef0 = -0.1)

# playing with gamma for the sigmoid kernel
gammas <- seq(from = 0, to = 5, by = 0.1)
res <- lapply(gammas, FUN = function(x) {
        classifier <- svm(formula = Purchased ~ .,
                          data = training_set,
                          type = "C-classification",
                          kernel = "sigmoid", gamma = x)
        y_pred <- predict(classifier, newdata = training_set[-3])
        cm <- table(training_set[, 3], y_pred)
        c(x,cm[[1]]+cm[[4]])
})

# varying brute force over gamma and coef0 entries
coef0s <- seq(from = -3, to = 3, by = 0.1)
combos <- expand.grid(gammas,coef0s)
res <- apply(combos, MARGIN = 1, FUN = function(x) {
        classifier <- svm(formula = Purchased ~ .,
                          data = training_set,
                          type = "C-classification",
                          kernel = "sigmoid", gamma = x[1], coef0 = x[2])
        y_pred <- predict(classifier, newdata = training_set[-3])
        cm <- table(training_set[, 3], y_pred)
        c(x,cm[[1]]+cm[[4]])        
})
best <- res[,which.max(res[3,])]
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "sigmoid", gamma = best[1], coef0 = best[2])

y_pred <- predict(classifier, newdata = test_set[-3])

# summary table of results
cm <- table(test_set[, 3], y_pred)
cm

# visualisation of the data to guess the outcome
library(ggplot2)
ggplot() + geom_point(aes(x = training_set[,1], y = training_set[,2]), fill = ifelse(training_set[3] == 1,"green","red"), shape = 21)

# visualise the results for the training set
display(training_set, title = "SVM (Training Set)")
display(test_set, title = "SVM (Test Set)")

display <- function(set,title) {
        library(ElemStatLearn)
        X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.02)
        X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.02)
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



