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

# Filtering the data to include only observations 1991-2006 and the arithmetic return columns
data <- data[,c(1,5:7)] %>%
  filter(as.Date('1991-06-30') <= Date) %>%
  filter(Date < as.Date('2007-06-30'))
names(data)[2:4] <- c('oil', 'gas', 'dry')

# Add column of averages to the dataframe
# Mean wasn't cooperating, so I computed this the old-fashioned way!
data$avg <- (data$oil + data$gas + data$dry) / 3

######################################################
#### Normal Distribution Estimation for 2006-2012 ####
######################################################

# Using the mean and std deviation from the raw data 1991-2006
est <- rnorm(6, mean = mean(data$avg), sd = sd(data$avg))

##################################################################
#### Tri Distribution Estimations for 2013-2015 and 2016-2019 ####
##################################################################

# From 2012 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
est2 <- rtri(3, min = 0.07, max = 0.22, mode = 0.0917)

# From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
est3 <- rtri(4, min = 0.02, max = 0.06, mode = 0.05)

#########################
#### Predicting 2020 ####
#########################

#### With the assumption of normality for 2006-2012 ####
hist_returns <- c(data$avg, est, est2, est3)

# 2020 is forecasted to follow the same increase distribution as from 2015 to 2019.
r <- rtri(n=10000, min = 0.02, max = 0.06, mode = 0.05)
P0 <- 0
P1 <- P0*(1+r)

mean(P1)
sd(P1)

hist(P1, breaks=50, main='2020 Return Distribution', xlab='Final Value')
abline(v = mean(P1), col = "red", lwd = 2)
abline(v = mean(P1) + sd(P1), col = "red", lwd = 2, lty = 2)
abline(v = mean(P1) - sd(P1), col = "red", lwd = 2, lty = 2)

#####################################
#### Kernal Estimation 2006-2012 ####
#####################################

Est.returns <- rkde(fhat=kde(data$avg, h=1), n=1000)
hist(Est.returns, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')
mean(Est.returns) # 0.191158
sd(Est.returns) # 1.035745

qqnorm(data$avg)

test = rep(1, 1000)
for (j in 1:1000) {
  # 2007-2012
  
  if (j == 1) {
    P0 <- costs$avg[16]
    r <- rkde(
      fhat = kde(
        all_changes,
        h = 1
      ),
      n = 1)
    P2 <- P0 * (1 + r) # 2007
  }
  else{
    P1 <- P2
    r <- rkde(
      fhat = kde(
        all_changes,
        h = 1
      ),
      n = 1)
    P2 <- P1 * (1 + r) # 2008-1012
  }
  test[j] <- P2
}

hist(test)

density_all_changes <- density(all_changes, bw="SJ-ste")
dac <- rkde(fhat=kde(all_changes, h=1), n=1)
P0*(1+dac)
hist(dac, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')

