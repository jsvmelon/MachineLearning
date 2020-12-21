source("display.R")
# Decision Tree Classification

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

# logistic regression classifier
classifier <- glm(formula = Purchased ~ ., family = binomial, data = training_set)
g0a <- display_ggplot2(training_set,classifier,"Logistic (Training Set)",logistic = TRUE)
g0b <- display_ggplot2(test_set,classifier,"Logistic (Test Set)",logistic = TRUE)

# Fitting Decision Tree classification to the training set
library(rpart)
classifier <- rpart(formula = Purchased ~ ., data = training_set)
g1a <- display_ggplot2(training_set,classifier,"Decision Tree (Training Set)")
g1b <- display_ggplot2(test_set,classifier,"Decision Tree (Test Set)")

# Fitting Random Forest to the training set
library(randomForest)
classifier <- randomForest(x = training_set[-3],
                           y = training_set$Purchased,
                           ntree = 10)
g2a <- display_ggplot2(training_set,classifier,"Random Forest (Training Set)")
g2b <- display_ggplot2(test_set,classifier,"Random Forest (Test Set)")

# K-NN classifier
classifier <- NULL
g3a <- display_ggplot2(training_set,classifier,"K-NN (Training Set)",TRUE)
g3b <- display_ggplot2(test_set,classifier,"K-NN (Test Set)",TRUE)

# Fitting Kernel SVM to the training set
library(e1071)
classifier <- svm(formula = Purchased ~ ., data = training_set, type = "C-classification", kernel = "radial")
g4a <- display_ggplot2(training_set,classifier,"SVM - Linear (Training Set)")
g4b <- display_ggplot2(test_set,classifier,"SVM - Linear (Test Set)")

# Naive Bayes
classifier <- naiveBayes(x = training_set[-3], y = training_set$Purchased)
g5a <- display_ggplot2(training_set,classifier,"Naive Bayes (Training Set)")
g5b <- display_ggplot2(test_set,classifier,"Naive Bayes (Test Set)")

# SVM: varying brute force over gamma and coef0 entries to find the best radial parameters
gammas <- seq(from = 0, to = 5, by = 0.1)
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
# find the best classifier in the set
best <- res[,which.max(res[3,])]
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "sigmoid", gamma = best[1], coef0 = best[2])
g6a <- display_ggplot2(training_set,classifier,"SVM - Radial Kernel (Training Set)")
g6b <- display_ggplot2(test_set,classifier,"SVM - Radial Kernel)(Test Set)")

library(gridExtra)

grid.arrange(g0a,g0b,
             g1a,g1b,
             g2a,g2b,
             g3a,g3b,
             g4a,g4b,
             g5a,g5b,
             g6a,g6b,
             nrow = 7)

grid.arrange(g0a,g1a,g2a,g3a,g4a,g5a,g6a,
             g0b,g1b,g2b,g3b,g4b,g5b,g6b,
             nrow = 2)
