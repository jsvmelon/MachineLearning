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

# Regression Model for Decision Tree Regression
library(rpart)
regressor <- rpart(formula = Salary ~ ., 
                   data = dataset,
                   control = rpart.control(minsplit = 1))

# Predicting a new result
y_pred <- predict(regressor, newdata = data.frame(Level = 6.5))

# visualise Decision Tree Regression
library(ggplot2)
x_grid <- seq(min(dataset$Level),max(dataset$Level),0.01)
plotly::ggplotly(
  ggplot() + 
    geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'blue') + 
    geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))), colour = 'darkviolet') +
    ggtitle("Decision Tree Regression") + 
    xlab("Level") + 
    ylab("Salary") 
  , dynamicTicks = TRUE)

# visualise polynomial
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'blue') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg)), colour = 'red')
  
