# upper confidence bound

# import the dataset 
dataset <- read.csv("Ads_CTR_Optimisation.csv")

# random approach
f <- function(counter) {
  random_reward <- sum(apply(dataset, MARGIN = 1, FUN = function(x) {
    reward <- x[sample(1:10,1)]
  }))
  if (counter == 1) return(random_reward)
  else return(c(random_reward, f(counter-1)))
}
random_reward <- mean(f(100))

# UCB approach
d <- 10
ads_selected <- integer()
numbers_of_selections <- integer(d)
sums_of_rewards <- integer(d)
total_reward <- 0

###
# initialisation
# the upper_bound calculation divides by numbers_of_selections which hence must not be 0
numbers_of_selections <- unlist(lapply(1:10,FUN = function(x) {
  numbers_of_selections[x] <- 1
}))

# because log(1) = 0 the upper bound will be 0 in the first round for all entries unless the average reward is different for any one entry
sums_of_rewards[sample(1:10,1)] <- 1

for (n in 1:10000) {
  ad <- 0
  max_upper_bound <- 0
  for (i in 1:d) {
    average_reward <- sums_of_rewards[i] / numbers_of_selections[i]
    upper_bound <- average_reward + sqrt(3/2 * log(n) / numbers_of_selections[i])

    if(upper_bound >= max_upper_bound) { 
      max_upper_bound <- upper_bound 
      ad <- i
    }
  }
  #if (ad == 0) { ad = sample(1:10,1) }
  ads_selected <- append(ads_selected, ad)
  numbers_of_selections[ad] <- numbers_of_selections[ad] + 1
  sums_of_rewards[ad] <- sums_of_rewards[ad] + dataset[n, ad]
  total_reward <- total_reward + dataset[n, ad]
}

###
# functional approach
# recursion takes lots of memory and the dataset is too large for this approach
# I could probably improve it by using global variable from within the function - but that would defeat the purpose imho

source("~/code/Machine Learning A-Z (Codes and Datasets)/Own Code/MachineLearning/Reinforcement Learning/functional UCB.R")

# initialisation: we are assuming each ad has been chosen once but with no reward 
# (the calculation of the upper bound has a division by number of selection - so can't be 0)
per_ad_data <- data.frame(number_of_selections = rep(1,10), sums_of_rewards = integer(10), upper_bound = integer(10))
ucb(dataset = dataset[1:500,], per_ad_data = per_ad_data, ads_selected = rep(1,10), 11)

### finished the functional approach

# visualise
hist(ads_selected, 
     col = "blue",
     main = "Histogram of ads selections",
     xlab = "Ads",
     ylab = "Number of times each ad was selected")
