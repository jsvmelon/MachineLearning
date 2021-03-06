display <- function(set,title) {
  library(ElemStatLearn)
  X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.02)
  X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.02)
  grid_set <- expand.grid(X1,X2)
  colnames(grid_set) = c("Age","EstimatedSalary")
  y_grid <- predict(classifier, newdata = grid_set, type = "class")
  
  plot(set[, -3],
       main = title,
       xlab = "Age" , ylab = "Estimated Salary",
       xlim = range(X1), ylim = range(X2))
  
  contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
  points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3", "tomato"))
  points(set, pch = 21, bg = ifelse(set[, 3] == 1, "green4", "red3"))
}

display_ggplot2 <- function(set,classsifier,title, KNN = FALSE, logistic = FALSE) {
  # not catching the case KNN and logistic both being TRUE ...
  library(ggplot2) ; library(class)
  X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.02)
  X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.02)
  grid_set <- expand.grid(X1,X2)
  colnames(grid_set) = c("Age","EstimatedSalary")
  if(KNN) y_grid <- knn(train = set[,-3], test = grid_set, cl = set[,3], 5)
  else if(logistic) {
    prob_set <- predict(classifier, type = "response", newdata = grid_set)
    y_grid <- ifelse(prob_set > 0.5, 1, 0)
  }
  else y_grid <- predict(classifier, newdata = grid_set, type = "class")
  
  ggplot() +
    geom_point(aes(x = grid_set[,1],y = grid_set[,2]), color = ifelse(y_grid == 1,"springgreen3","tomato")) +
    geom_point(aes(x = set[,1], y = set[,2]), fill = ifelse(set[,3] == 1, "green4", "red3"), shape = 21) +
    ggtitle(title) + xlab("Age") + ylab("Estimated Salary")
}
