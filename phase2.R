library(graphics)
library(ks)
library(readxl)
library(dplyr)
library(lubridate)
library(EnvStats)
library(triangle)

set.seed(12345)

###################
#### Data Prep ####
###################

# I created a copy of the data file from Moodle and included only the second sheet, which is why I didn't need to indicate sheet here.
data <- read_excel("Analysis_Data.xlsx")

#### Costs ####

# Filtering the data to include only observations 1991-2006 and the arithmetic return columns
costs <- data[, c(1:4)] %>%
  filter(as.Date('1991-06-30') <= Date) %>%
  filter(Date < as.Date('2007-06-30'))
names(costs)[2:4] <- c('oil', 'gas', 'dry')

# Add column of averages to the dataframe
# Mean wasn't cooperating, so I computed this the old-fashioned way!
costs$avg <- (costs$oil + costs$gas + costs$dry) / 3

#### Changes ####

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

#### Estimated Oil Prices ####
pred <- read_excel('Projection_Data.xlsx')
pred$mean = (pred$High + pred$Low)/2

#########################
#### For Correlation ####
#########################

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

##################################################
#### Simulation Assuming Normality (Dry Well) ####
##################################################

P2020 <- rep(1, 10000)

wells <- 1 # No. of wells
years <- 1

for (i in 1:10000) {
  # Simulating Year 0 costs
  # Uncertain assumptions include leased acres per well, which are Normally 
  # distributed with a mean of 600 and a standard deviation of 50 acres per well
  # (lower number of wells means lower number of acres to buy); the price per 
  # acre is $960.
  lease <- 960*rnorm(
    n = 1,
    mean = 600*wells,
    sd = 50*wells
  )/1000
  # The number of seismic sections per well is Normally distributed with a mean
  # of 3 sections and a standard deviation of 0.35 per well (lower number of wells
  # means lower number of sections to purchase data on); the seismic costs per 
  # section area $43,000.
  seismic <- 43000*rnorm(
    n = 1,
    mean = 3*wells,
    sd = 0.35*wells
  )/1000
  # The project team annual costs in salary and benefits per well depends on 
  # the time that the project team spends on the well. We believe the salary 
  # and benefit cost is best represented by a triangular distribution, with a 
  # most likely cost as $215,000, with a minimum of $172,000 and a maximum of 
  # $279,500. This will remain constant across the lifetime of a well, but would
  # potentially be different for different wells. These costs are incurred 
  # during Year 0 as well for drilling, but stop after Year 0 if the well is dry.
  prof <- years*rtriangle(n = 1,
                          a = 172000,
                          b = 279000,
                          c = 215000)/1000
  expenses <- lease + seismic + prof
  
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
      r <- rtriangle(n = 1,
                     a = 0.07,
                     b = 0.22,
                     c = 0.0917)
      P4 <- P2 * (1 - r) # 2013
    }
    else{
      P3 <- P4
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtriangle(n = 1,
                     a = 0.07,
                     b = 0.22,
                     c = 0.0917)
      P4 <- P3 * (1 - r) # 2014-2015
    }
    
  }
  for (m in 1:5) {
    # 2016-2020
    
    if (m == 1) {
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      r <- rtriangle(n = 1,
                     a = 0.02,
                     b = 0.06,
                     c = 0.05)
      P6 <- P4 * (1 + r) # 2016
    }
    else{
      P5 <- P4
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      # 2020 is forecasted to follow the same increase distribution as from 2015 to 2019.
      r <- rtriangle(n = 1,
                     a = 0.02,
                     b = 0.06,
                     c = 0.05)
      P6 <- P5 * (1 + r) #2018-2020
    }
    
  }
  
  P2020[i] <- P6 + expenses
  
}

hist(P2020) # Cost of a dry well in 2020


##################################################
#### Simulation Assuming Normality (Wet Well) ####
##################################################
npv_dist <- rep(1, 10000)

wells <- 1 # No. of wells
years <- 15 # Years being simulated

for (i in 1:10000) {
  
  ################ First we need to calculate the predicted cost of drilling the well in 2020. ####################
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
      r <- rtriangle(n = 1,
                     a = 0.07,
                     b = 0.22,
                     c = 0.0917)
      P4 <- P2 * (1 - r) # 2013
    }
    else{
      P3 <- P4
      # From 2013 to 2015 costs tended to decrease on average by 9.17% per year with a maximum of 22% and minimum of 7%.
      r <- rtriangle(n = 1,
                     a = 0.07,
                     b = 0.22,
                     c = 0.0917)
      P4 <- P3 * (1 - r) # 2014-2015
    }
    
  }
  for (m in 1:5) {
    # 2016-2020
    
    if (m == 1) {
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      r <- rtriangle(n = 1,
                     a = 0.02,
                     b = 0.06,
                     c = 0.05)
      P6 <- P4 * (1 + r) # 2016
    }
    else{
      P5 <- P4
      # From 2015 to 2019 costs tended to increase on average by 5% per year with a maximum of 6% and minimum of 2%.
      # 2020 is forecasted to follow the same increase distribution as from 2015 to 2019.
      r <- rtriangle(n = 1,
                     a = 0.02,
                     b = 0.06,
                     c = 0.05)
      P6 <- (P5 * (1 + r))*1000 #2018-2020
    }
    
  }
  
  drill_cost <- P6
  
  ############### Now that we have a distibution of the drilling cost. We need to simulate additional expenses. ##############

  # Uncertain assumptions include leased acres per well, which are Normally 
  # distributed with a mean of 600 and a standard deviation of 50 acres per well
  # (lower number of wells means lower number of acres to buy); the price per 
  # acre is $960.
  lease <- 960*rnorm(
    n = 1,
    mean = 600*wells,
    sd = 50*wells
  )
  
  # The number of seismic sections per well is Normally distributed with a mean
  # of 3 sections and a standard deviation of 0.35 per well (lower number of wells
  # means lower number of sections to purchase data on); the seismic costs per 
  # section area $43,000.
  seismic <- 43000*rnorm(
    n = 1,
    mean = 3*wells,
    sd = 0.35*wells
  )
  
  # If it is determined that there is oil present in the reservoir 
  # (and we have not drilled a dry hole), engineers must prepare the
  # well to produce oil at the optimum sustainable rates. For this 
  # particular well, we hypothesize that his cost is Normally 
  # distributed with a mean of $390,000 and a standard deviation of $50,000.
  complete <- wells*rnorm(
    n = 1,
    mean = 390000,
    sd = 50000
  )
  
  expenses <- lease + seismic + complete + drill_cost
  
  ############ Next, we have to predict the rate of oil production and # of barrels produced per year. ###################
  
  # The initial production rate tested from the drilled well. This is the rate the oil is produced at Year 1.
  # The IP’s follow a Lognormal distribution with a mean of 420 BOPD and a standard deviation of 120 BOPD. 
  ip <- rlnorm(
    n = 10000,
    meanlog = 6,
    sdlog = 0.28
  )
  
  # A declining production rate that describes the annual decrease in production from the beginning of the year to
  # the end of the same year. To simplify the problem, we will assume each well has the same decline rate for every year of its life, 
  # but this could be different across wells.
  # The rate of decline is Uniformly distributed between 15 and 32 percent.
  decline <- runif(
    n = 10000,
    min = 0.15,
    max = 0.32
  )
  
  # We incorporate an additional constraint in the production model; we have imposed acorrelation coefficient of 0.64 
  # between the IP and the decline rate assumptions that are drawn from their respective distributions during each trial of the simulation.
  R <- matrix(data = cbind(1, 0.64, 0.64, 1), nrow = 2)
  U <- t(chol(R))
  
  Both.r <- cbind(standardize(decline), standardize(ip))
  ip_decline.r <- U %*% t(Both.r)
  ip_decline.r <- t(ip_decline.r)
  
  final.ip_decline.r <- cbind(destandardize(ip_decline.r[,1], decline),
                      destandardize(ip_decline.r[,2], ip))
  
  summary(final.ip_decline.r)
  
  ############## Knowing these figures, we can estimate revenue from sales minus operational and preofessional costs #############
  
  revenue <- rep(1, years)
  wacc <- rep(1, years)
  for (y in 1:years){
    
    decline <- final.ip_decline.r[i,1]
    ip <- final.ip_decline.r[i,2]
    
    ppb <- rtriangle( # Price per barrel
      n = 1,
      a = pred$Low[y],
      b = pred$High[y],
      c = pred$mean[y]
    )
    
    if (y == 1){
      year_end = (1 - decline)*ip
      barrels = 365*(ip+year_end)/2
    }
    else{
      year_begin = year_end
      year_end = (1-decline)*year_begin
      barrels = 365*(year_begin+year_end)/2
    }
    sales <- ppb*barrels
   
    # Oil companies must purchase leases from mineral interest holders. Along with paying cash to retain the 
    # drilling and production rights to a property for a specified time period, the lessee also generally retains
    # some percentage of the oil revenue produced in the form of a royalty. The percentage that the producing 
    # company retains after paying all royalties is the net revenue interest (NRI). Our model represents a typical West 
    # Texas scenario with an assumed NRI distributed Normally with a mean of 75% and a standard deviation of 2%. 
    # This calculation is done per well for the entire life of the well.
    nri <- rnorm(
      n = 1,
      mean = 0.75,
      sd = 0.02
    )
    
    # State taxes levied on produced oil and gas are assumed to be a constant value of 4.6% of revenue. Taxes are applied after the NRI.
    income <- sales*nri - (0.046*sales)
    
    # Companies must pay for manpower and hardware involved in the production process. 
    # These expenses are generally described as a dollar amount per barrel. A reasonable West Texas cost 
    # would be Normally distributed with a mean of $2.25 per barrel with a standard deviation of $0.30 per 
    # barrel. The expenses would be the same for every well in a given year, but could change from year 
    # to year with the distribution above.
    op <- rnorm(
      n = 1,
      mean = 2.25,
      sd = 0.3
    )
    opcost <- barrels*op
    
    # The project team annual costs in salary and benefits per well depends on 
    # the time that the project team spends on the well. We believe the salary 
    # and benefit cost is best represented by a triangular distribution, with a 
    # most likely cost as $215,000, with a minimum of $172,000 and a maximum of 
    # $279,500. This will remain constant across the lifetime of a well, but would
    # potentially be different for different wells. These costs are incurred 
    # during Year 0 as well for drilling, but stop after Year 0 if the well is dry.
    prof <- rtriangle(n = 1,
                      a = 172000,
                      b = 279000,
                      c = 215000)
    
    revenue[y] <- income - opcost - prof
    
    wacc[y] <- (1.1)**y
  }
  
  sum_revenue <- rep(1, years)
  for (x in 1:years){
    sum_revenue[x] <- revenue[x]/wacc[x]
  }
  
  npv <- -expenses + sum(sum_revenue)
  npv_dist[i] <- (npv/1000000)
  
}

hist(npv_dist) # These values are in millions
median(npv_dist)

