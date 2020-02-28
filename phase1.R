library(graphics)
library(ks)
library(readxl)
library(dplyr)
library(lubridate)
library(EnvStats)

set.seed(1234)

###################
#### Data Prep ####
###################

# I created a copy of the data file from Moodle and included only the second sheet, which is why I didn't need to indicate sheet here.
data <- read_excel("Analysis_Data.xlsx")

###############
#### Costs ####
###############

# Filtering the data to include only observations 1991-2006 and the arithmetic return columns
costs <- data[, c(1:4)] %>%
  filter(as.Date('1991-06-30') <= Date) %>%
  filter(Date < as.Date('2007-06-30'))
names(costs)[2:4] <- c('oil', 'gas', 'dry')

# Add column of averages to the dataframe
# Mean wasn't cooperating, so I computed this the old-fashioned way!
costs$avg <- (costs$oil + costs$gas + costs$dry) / 3

#################
#### Changes ####
#################

# Filtering the data to include only observations 1991-2006 and the arithmetic return columns
changes <- data[, c(1, 5:7)] %>%
  filter(as.Date('1991-06-30') <= Date) %>%
  filter(Date < as.Date('2007-06-30'))
names(changes)[2:4] <- c('oil', 'gas', 'dry')

# Add column of averages to the dataframe
# Mean wasn't cooperating, so I computed this the old-fashioned way!
changes$avg <- (changes$oil + changes$gas + changes$dry) / 3

#### Returns ####
returns <- costs$avg*(1 + changes$avg)

#######################################
#### Simulation Assuming Normality ####
#######################################

P2020 <- rep(1, 10000)

for (i in 1:10000) {
  for (j in 1:6) {
    # 2007-2012
    
    if (j == 1) {
      P0 <- costs$avg[16]
      r <-
        rnorm(
          n = 1,
          mean = mean(changes$avg),
          sd = sd(changes$avg)
        )
      P2 <- P0 * (1 + r) # 2007
    }
    else{
      P1 <- P2
      r <-
        rnorm(
          n = 1,
          mean = mean(changes$avg),
          sd = sd(changes$avg)
        )
      P2 <- P1 * (1 + r) # 2008-1012
    }
    
  }
  
  for (k in 1:3) {
    # 2013-2015
    
    if (k == 1) {
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtri(1,
                min = 0.07,
                max = 0.22,
                mode = 0.0917)
      P4 <- P2 * (1 - r) # 2013
    }
    else{
      P3 <- P4
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtri(1,
                min = 0.07,
                max = 0.22,
                mode = 0.0917)
      P4 <- P3 * (1 - r) # 2014-2015
    }
    
  }
  for (m in 1:5) {
    # 2016-2020
    
    if (m == 1) {
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      r <- rtri(1,
                min = 0.02,
                max = 0.06,
                mode = 0.05)
      P6 <- P4 * (1 + r) # 2016
    }
    else{
      P5 <- P4
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      # 2020 is forecasted to follow the same increase distribution as from 2015 to 2019.
      r <- rtri(1,
                min = 0.02,
                max = 0.06,
                mode = 0.05)
      P6 <- P5 * (1 + r) #2018-2020
    }
    
  }
  
  P2020[i] <- P6
  
}

quantile(P2020)
mean(P2020) # 3955.332
sd(P2020) # 1147.395

#### Graph ####
hist(P2020, main = 'Histogram of 2020 Cost Simulation', xlab = "Cost (in thousands)", border = F, col = rgb(0.2,0.8,0.5,0.5))

#################################################
#### Simulation with Kernal Density Estimate ####
#################################################

# Previously the Price Analysis group has worked under the assumption that these arithmetic changes
# from one year to the next from 2006 to 2012 follow a Normal distribution.
# Use QQ-plots or formal tests to see if you agree.

# Because of the direction in the next bullet point, I'm going to plot all of the arithmetic changes.
# It doesn't make sense to plot the simulated values for 2007-2012 from above because they were selected with
# rnorm and would, of course, be normal.
all_changes <- c(changes$oil, changes$gas, changes$dry)
all_costs <- c(costs$oil, costs$gas, costs$dry)
all_returns <- all_changes*(1 + all_costs)
qqnorm(all_changes) # Looks very normal to me!!

# The Price Analysis group would also like you to build a kernel density estimate of the distribution of 
# arithmetic changes using the 48 observations described above (1990 – 2006). Use this kernel density 
# to simulate the changes from 2006 to 2012 as well.

density(all_changes) # bw:0.06718

P2020kde <- rep(1, 10000)

for (i in 1:10000) {
  for (j in 1:6) {
    # 2007-2012
    
    if (j == 1) {
      P0 <- costs$avg[16]
      r <- rkde(fhat = kde(all_changes, h = 0.06718), n = 1)
      P2 <- P0 * (1 + r) # 2007
    }
    else{
      P1 <- P2
      r <- rkde(fhat = kde(all_changes, h = 0.06718), n = 1)
      P2 <- P1 * (1 + r) # 2008-1012
    }
    
  }
  
  for (k in 1:3) {
    # 2013-2015
    
    if (k == 1) {
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtri(1,
                min = 0.07,
                max = 0.22,
                mode = 0.0917)
      P4 <- P2 * (1 - r) # 2013
    }
    else{
      P3 <- P4
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtri(1,
                min = 0.07,
                max = 0.22,
                mode = 0.0917)
      P4 <- P3 * (1 - r) # 2014-2015
    }
    
  }
  for (m in 1:5) {
    # 2016-2020
    
    if (m == 1) {
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      r <- rtri(1,
                min = 0.02,
                max = 0.06,
                mode = 0.05)
      P6 <- P4 * (1 + r) # 2016
    }
    else{
      P5 <- P4
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      # 2020 is forecasted to follow the same increase distribution as from 2015 to 2019.
      r <- rtri(1,
                min = 0.02,
                max = 0.06,
                mode = 0.05)
      P6 <- P5 * (1 + r) #2018-2020
    }
    
  }
  
  P2020kde[i] <- P6
  
}

mean(P2020kde) # 3964.344
sd(P2020kde) # 1698.779
quantile(P2020kde)

#### Graph ####
hist(P2020kde, main = 'Histogram of 2020 Cost Simulation with Kernel Density Estimation', xlab = "Cost (in thousands)", border = F, col = rgb(0.2,0.8,0.5,0.5))
