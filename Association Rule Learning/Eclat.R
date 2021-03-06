# Eclat

library(arules)
dataset <- read.transactions("Market_Basket_Optimisation.csv", sep = ",", rm.duplicates = TRUE)

summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# training Eclat on the dataset
parameter <- list(support = 0.004, minlen = 2)
rules <- eclat(data = dataset, parameter = parameter)

# visualising the results
inspect(sort(rules, by = "support")[1:10])
