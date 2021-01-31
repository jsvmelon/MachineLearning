# this uses reduce instead of the inner for loop
red <- Reduce(
  f = function(a,b){
    random_beta <- rbeta(1, shape1 = b[[1]] + 1, shape2 = b[[2]] + 1)
    if (random_beta > a$max) list(max = random_beta, index = a$cindex+1, cindex = a$cindex+1)
    else list(max = a$max, index = a$index, cindex = a$cindex+1)
  }, x = numbers_of_rewards, init = list(max = 0, index = 0, cindex = 0)
)
ad <- red$index

# this is the original for loop solution
for (i in 1:d) {
  random_beta <- rbeta(1, shape1 = numbers_of_rewards_1[i] + 1, shape2 = numbers_of_rewards_0[i] + 1)
  
  if(random_beta >= max_random) { 
    max_random <- random_beta
    ad <- i
  }
}

