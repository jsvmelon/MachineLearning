# natural language processing - Bag of Words

setwd("~/code/Machine Learning A-Z (Codes and Datasets)/Part 7 - Natural Language Processing/Section 36 - Natural Language Processing/R")

# importing the dataset
dataset_original <- read.delim("Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)

# clean text to remove irrelevant words
library(tm)
library(SnowballC)

corpus <- VCorpus(VectorSource(dataset_original$Review))
corpus <- tm_map(x = corpus, content_transformer(tolower)) # make sure everything is in lower case
corpus <- tm_map(x = corpus, removeNumbers)                # remove numbers from the corpus
corpus <- tm_map(x = corpus, removePunctuation)            # remove punctuation
corpus <- tm_map(x = corpus, removeWords, stopwords())     # remove unhelpful words
corpus <- tm_map(x = corpus, stemDocument)                 # replace words with their root
corpus <- tm_map(x = corpus, stripWhitespace)              # remove extra spaces

# creating the Bag of Words model
dtm <- DocumentTermMatrix(corpus)                          # create the sparse matrix
dtm <- removeSparseTerms(dtm, 0.999)                       # keep 99.9% of the most frequent words 

# transform the data format so that it works for Random Tree Classification
dataset <- as.data.frame(as.matrix(dtm))
dataset$Liked <- dataset_original$Liked

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Liked,SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#
# Random Tree Classification
#

# Fitting Random Forest to the training set
library(randomForest)
training_set$Liked <- as.factor(training_set$Liked)
classifier <- randomForest(x = training_set[-692], y = training_set$Liked, ntree = 500)

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- predict(classifier, newdata = test_set[-692])

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm 

#
# Naive Bayes 
#

library(e1071)
classifier <- naiveBayes(x = training_set[-692],
                         y = training_set$Liked)

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- predict(classifier, newdata = test_set[-692])

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm

#
# Decision Tree
#

library(rpart)
classifier <- rpart(formula = Liked ~ ., data = training_set)

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- predict(classifier, newdata = test_set[-692], type = "class")

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm

#
# Logistic Regression
#

classifier <- glm(formula = Liked ~ ., family = binomial, data = training_set)

# Predicting the test set results
prob_pred <- predict(classifier, type = "response", newdata = test_set[-692])

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- ifelse(prob_pred > 0.5, 1, 0) 

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm

#
# K-NN
#

library(class)
y_pred = knn(train = training_set[,-692], test = test_set[,-692], cl = training_set[,692], 5)

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm

#
# Kernel SVM
#

library(e1071)
classifier <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "radial")
classifier <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "linear")
classifier <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "polynomial", degree = 3)
classifier <- svm(formula = Liked ~ ., data = training_set, type = "C-classification", kernel = "sigmoid")

# normalise predictions to be 0 or 1 instead of the probability
y_pred <- predict(classifier, newdata = test_set[-692])

# making the confusion matrix
cm <- table(test_set[, 692], y_pred)
cm

library(C50)
classifier <- C5.0(x = training_set[, -692], y = as.factor(training_set$Liked), rules = TRUE)
y_pred <- predict(classifier, newdata = test_set[-692])
cm <- table(test_set[, 692], y_pred)
cm

# metrics to judge the quality of a model
accuracy <- (cm[1] + cm[4]) / (sum(cm))
precision <- cm[4] / (cm[4]+cm[3])
recall <- cm[4] / (cm[4] + cm[2])
f1_score <- 2 * precision * recall / (precision + recall)

