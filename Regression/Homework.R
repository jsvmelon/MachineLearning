# Import the dataset
dataset <- read.csv("Data.csv")

# missing data using the mean
dataset$Age <- ifelse(is.na(dataset$Age),
                      no = dataset$Age, 
                      yes = ave(dataset$Age,FUN = function(x) mean(x,na.rm = TRUE)))

dataset$Salary <- ifelse(is.na(dataset$Salary), 
                         yes = ave(dataset$Salary, FUN = function(x) mean(x,na.rm = TRUE)),
                         no = dataset$Salary)

# encode categorical variables
dataset$Country <- factor(x = dataset$Country, 
                          levels = c("France","Spain","Germany"),
                          labels = c(1:3))

dataset$Purchased <- factor(x = dataset$Purchased,
                            levels = c("No" ,"Yes"),
                            labels = c(0:1))

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased,SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
training_set[,2:3] <- scale(training_set[,2:3])
test_set[,2:3] <- scale(test_set[,2:3])
