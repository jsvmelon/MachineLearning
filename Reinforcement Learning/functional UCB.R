ucb <- function(dataset, per_ad_data, ads_selected, round_n) {
  # calculate the current max upper bound and which ad has it
  upbs <- lapply(1:10, FUN = function(i,n,per_ad_data) {
    average_reward <- per_ad_data$sums_of_rewards[[i]] / per_ad_data$number_of_selections[[i]]
    upper_bound <- average_reward + sqrt(3/2 * log(n) / per_ad_data$number_of_selections[[i]])
  },round_n,per_ad_data)
  max_upper_bound <- max(unlist(upbs))
  index_of_max <- which(upbs == max_upper_bound)
  if(length(index_of_max) > 1) index_of_max <- sample(index_of_max,1)

  per_ad_data$number_of_selections[[index_of_max]] <- per_ad_data$number_of_selections[[index_of_max]] + 1
  per_ad_data$sums_of_rewards[[index_of_max]] <- per_ad_data$sums_of_rewards[[index_of_max]] + dataset[1,index_of_max]
  ads_selected <- append(ads_selected, index_of_max)
  
  if(nrow(dataset) > 1) { 
    ucb(dataset = dataset[2:nrow(dataset),], per_ad_data = per_ad_data, ads_selected = ads_selected, round_n = round_n + 1) 
  }
  else return(per_ad_data)
}