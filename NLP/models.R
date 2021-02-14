# each function returns a confusion matrix

getCMSs <- function() {
  list_of_functions <-list()
  
  list_of_functions$random_forest <- function(training_set, test_set) {
    library(randomForest)
    training_set$Liked <- as.factor(training_set$Liked)
    test_set$Liked <- as.factor(test_set$Liked)
    classifier <- randomForest(x = training_set[-692], y = training_set$Liked, ntree = 500)
    y_pred <- predict(classifier, newdata = test_set[-692])
    return(table(test_set[, 692], y_pred)) 
  }
  
  list_of_functions$naive_bayes <- function(training_set, test_set) {
    library(e1071)
    training_set$Liked <- as.factor(training_set$Liked)
    test_set$Liked <- as.factor(test_set$Liked)
    classifier <- naiveBayes(x = training_set[-692], y = training_set$Liked)
    y_pred <- predict(classifier, newdata = test_set[-692])
    return(table(test_set[, 692], y_pred))
  }
  
  list_of_functions$decision_tree <- function(training_set, test_set) {
    library(rpart)
    training_set$Liked <- as.factor(training_set$Liked)
    test_set$Liked <- as.factor(test_set$Liked)
    classifier <- rpart(formula = Liked ~ ., data = training_set)
    y_pred <- predict(classifier, newdata = test_set[-692], type = "class")
    return(table(test_set[, 692], y_pred))
  }
  
  list_of_functions$logistic_regression <- function(training_set, test_set) {
    classifier <- glm(formula = Liked ~ ., family = binomial, data = training_set)
    prob_pred <- predict(classifier, type = "response", newdata = test_set[-692])
    y_pred <- ifelse(prob_pred > 0.5, 1, 0) 
    return(table(test_set[, 692], y_pred))
  }
  
  list_of_functions$knn_cm <- function(training_set, test_set) {
    library(class)
    y_pred = knn(train = training_set[,-692], test = test_set[,-692], cl = training_set[,692], k = 5)
    return(table(test_set[, 692], y_pred))
  }
  
  list_of_functions$c50_cm <- function(training_set, test_set) {
    library(C50)
    classifier <- C5.0(x = training_set[, -692], y = as.factor(training_set$Liked), rules = TRUE)
    y_pred <- predict(classifier, newdata = test_set[-692])
    return(table(test_set[, 692], y_pred))
  }
  
  kernel_svm <- function(training_set, test_set) {
    library(e1071)
    classifier <- list()
    classifier$kernel_svm_radial <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "radial")
    classifier$kernel_svm_linear <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "linear")
    classifier$kernel_svm_polynomial <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "polynomial", degree = 3)
    classifier$kernel_svm_sigmoid <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "sigmoid")
    return(lapply(classifier,FUN = function(classifier) {
      y_pred <- predict(classifier, newdata = test_set[-692])
      table(test_set[, 692], y_pred)
    }))
  }
  
  # create a list of confusion matrixes
  cms <- lapply(list_of_functions, FUN = function(f) {cm <- f(training_set, test_set)})
  
  # list of confusion matrixes from kernel_svm
  l <- kernel_svm(training_set, test_set)
  
  # concatenate the lists
  cms <- c(cms,l)
  
  return(cms)
}
