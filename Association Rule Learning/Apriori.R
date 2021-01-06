# Apriori Association Rule

#dataset <- read.csv("Market_Basket_Optimisation.csv", header = FALSE)

library(arules)
dataset <- read.transactions("Market_Basket_Optimisation.csv", sep = ",", rm.duplicates = TRUE)

summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# training apriori on the dataset
parameter <- list(support = 0.004, confidence = 0.2)
rules <- apriori(data = dataset, parameter = parameter)

# visualising the results
inspect(sort(rules, by = "lift")[1:10])
