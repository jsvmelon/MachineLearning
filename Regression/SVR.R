# Data Preprocessing Template

# Importing the dataset
dataset <- read.csv('Position_Salaries.csv')
dataset <- dataset[2:3]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Regression Model
# TODO: Create Regression Model
library(e1071)
regressor <- svm(formula = Salary ~ ., data = dataset, type = "eps-regression")

# Predicting a new result
y_pred <- predict(regressor, newdata = data.frame(Level = 6.5))

# visualise SVR
library(ggplot2)
x_grid <- seq(min(dataset$Level),max(dataset$Level),0.1)
plotly::ggplotly(
  ggplot() + 
    geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'blue') + 
    geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))), colour = 'darkviolet') +
    ggtitle("SVR") + 
    xlab("Level") + 
    ylab("Salary") 
  , dynamicTicks = TRUE)
