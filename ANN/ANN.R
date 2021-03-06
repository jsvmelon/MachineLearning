setwd("~/code/Machine Learning A-Z (Codes and Datasets)/Part 8 - Deep Learning/Section 39 - Artificial Neural Networks (ANN)/R")

# Artificial Neural Networks

# Import the dataset
dataset <- read.csv("Churn_Modelling.csv")
dataset <- dataset[, 4:14] # this is just an example we might need from time to time

# encode categorical variable as factor
dataset$Geography <- as.numeric(factor(dataset$Geography, levels = c("France","Spain","Germany"), labels = c(1,2,3)))
dataset$Gender <- as.numeric(factor(dataset$Gender, levels = c("Female","Male"), labels = c(1,2)))

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Exited,SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
training_set[-11] <- scale(training_set[-11])
test_set[-11] <- scale(test_set[-11])

# fitting ANN to the training set
library(h2o)
h2o.init(nthreads = -1)
classifier <- h2o.deeplearning(y = "Exited", 
                               training_frame = as.h2o(training_set),
                               activation = "Rectifier",
                               hidden = c(6,6),
                               epochs = 200,
                               train_samples_per_iteration = 1)

# normalise predictions to be 0 or 1 instead of the probability
prob_pred <- h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred <- as.vector(prob_pred > 0.5)

# making the confusion matrix
cm <- table(test_set[, 11], y_pred)
cm
accuracy <- (cm[1] + cm[4]) / (sum(cm))
accuracy

h2o.shutdown()
