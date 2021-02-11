# Thompson Sampling

# import the dataset 
dataset <- read.csv("Ads_CTR_Optimisation.csv")

###
# initialisation
d <- 10
ads_selected <- integer()
total_reward <- 0
numbers_of_rewards <- as.data.frame(t(data.frame(integer(d),integer(d))))

numbers_of_rewards <- Reduce(f = function(numbers_of_rewards, row){
  # calculate rbeta for all ads; select the ad with maximum rbeta result
  ad <- which.max(
    apply(X = numbers_of_rewards, MARGIN = 2, FUN = function(x){
      rbeta(1, shape1 = x[[1]] + 1, shape2 = x[[2]] + 1)
    })
  )
  
  ads_selected <- append(ads_selected, ad)
  reward <- row[ad]
  if (reward == 1) {
    numbers_of_rewards[1,ad] <- numbers_of_rewards[1,ad] + 1
  } else {
    numbers_of_rewards[2,ad] <- numbers_of_rewards[2,ad] + 1
  }
  numbers_of_rewards
}, x = as.data.frame(t(dataset)), init = numbers_of_rewards)
total_reward <- sum(numbers_of_rewards[1,])

# visualise
hist(ads_selected, 
     col = "blue",
     main = "Histogram of ads selections",
     xlab = "Ads",
     ylab = "Number of times each ad was selected")
