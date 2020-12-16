# Import the dataset
dataset <- read.csv("50_Startups.csv")

# dataset <- dataset[, 2:3] # this is just an example we might need from time to time

# create the dummy variables
dataset$State <- factor(x = dataset$State, 
                        levels = c("New York","California","Florida"),
                        labels = c(1:3))

# split the dataset in training and test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit,SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# feature scaling (age and salary are not on the same numeric scale)
# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

# fitting the model with backward elimination
# The '.' is a shortcut for: R.D.Spend + Administration + Marketing.Spend + State
r1 <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, dataset)
r2 <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, dataset)
r3 <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend, dataset)
r4 <- lm(formula = Profit ~ R.D.Spend, dataset)

# as a recursive function
# SL - significance level
# formula - the formula for the linear model as used in the function lm
backwardElimination <- function(dataset, SL, formula) {
  regressor <- lm(formula = formula, data = dataset)
  pValues <- summary(regressor)$coefficients[,4]
  pValues <- pValues[2:length(pValues)] # we never want to eliminate the intercept
  if(length(pValues) < 2) return(regressor) 
  
  candidateValue <- max(pValues) # candidate value for elimination
  if (candidateValue > SL) {
    pos <- which(pValues == candidateValue)
    dataColumn <- regressor$assign[pos]+1 # we removed the intercept so +1 (this is mapping to the dataset - important in case of dummy variables)
    regressorReturn <- backwardElimination(dataset[,-dataColumn],SL,formula)
    
    # this is an improvement using adjusted r squared to judge if the elimination that was carried out in the next step did lead to a better model or not
    rqReturn <- summary(regressorReturn)$adj.r.squared
    rq <- summary(regressor)$adj.r.squared
    if (rqReturn > rq) return(regressorReturn)
    else return(regressor)
  } else { # nothing more to eliminate
    return(regressor)
  }
}

regressor <- backwardElimination(dataset,0.05, formula = Profit ~ .)

# predict
y_pred <- predict(r4, newdata = test_set)

library(ggplot2)
g <- ggplot() +
      geom_line(aes(x = test_set$R.D.Spend, y = y_pred), colour = 'blue') +
      geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit), colour = 'red') + 
      xlab("R&D Spend") +
      ylab("Profit") +
      ggtitle("R&D Spend vs Profit", subtitle = "Or how to fly to the Moon")
plotly::ggplotly(p = g, dynamicTicks = TRUE)

