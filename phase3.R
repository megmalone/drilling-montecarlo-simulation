library("truncnorm")
library(dplyr)

num_simulations <- 100000
prop_dist <- rep(1, num_simulations)
prob_hydro_dist <- rep(1,num_simulations)
prob_resevoir_dist <- rep(1,num_simulations)

for (i in 1:num_simulations) {
  # The total number of planned wells is assumed a Uniform distribution between 10 and 30
  tot_wells <- runif(1, min=10, max=30)

  # Dry-Hole Risk Factor Mean Standard Dev. Min. Max. 
  # Hydrocarbons 99% 5% 0% 100% 
  # Structure 100% 0% 100% 100% 
  # Reservoir 80% 10% 0% 100% 
  # Seal 100% 0% 100% 100%
  hydro <- rtruncnorm(1, a=0, b=1, mean = 0.99, sd = 0.05)
  prob_hydro_dist[i] <- hydro
  resevoir <- rtruncnorm(1, a=0, b=1, mean = 0.8, sd = 0.1)
  prob_resevoir_dist[i] <- resevoir

  # PPW = PH * PR * PSeal * PStructure
  # PSeal = PStructure = 1
  prob <- hydro * resevoir

  # If you know the probability of a producing well (randomly drawn from a combination 
  # of truncated Normal distributions as above), then you can simulate the number of 
  # producing wells using a combination of a Uniform distribution for the count and a 
  # Bernoulli distribution for whether the well is producing (wet) or dry. The output 
  # from a Bernoulli distribution (Binomial distribution with n = 1) is either a 1 or 0.
  wells <- rbinom(tot_wells,1,prob)
  
  prop_wet <- sum(wells) / length(wells)
  prop_dist[i] <- prop_wet
}

# Provide histogram of proportion of wells producing,
# probability of hydrocarbons, probability of resevoir
hist(prop_dist)
hist(prob_hydro_dist,breaks=40)
hist(prob_resevoir_dist,breaks=40)

# 5% VaR of proportion of wells producing
VaR <- unname(quantile(prop_dist, c(0.05)))[[1]]

# 5% CVaR of proportion of wells producing
# Filter data to only bottom 5%
VaR_data <- prop_dist[prop_dist <= VaR]
CVaR <- mean(VaR_data)
