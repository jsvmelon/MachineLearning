# Import the dataset
dataset <- read.csv("Salary_Data.csv")
# dataset <- dataset[, 2:3] # this is just an example we might need from time to time

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary,SplitRatio = 2/3)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

# fitting Simple Linear Regression
regressor <- lm(formula = Salary ~ YearsExperience,
                data = training_set)

# predict the test results
y_pred  <- predict(regressor, newdata = test_set)

# visualise results
library(ggplot2)
g1 <- ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), colour = 'black') +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), colour = 'red') +             # added this line in addition to the tutorial
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience') +
  xlab('Years of Experience') + 
  ylab('Salary')

plotly::ggplotly(g1)

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of Experience') + 
  ylab('Salary')
