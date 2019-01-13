# Jordan Totten
# MSDS 451
# 1/13/2019
#
# Simulation warm-up
# 
# Problem 4: what is the probability that the value of the stock will 
# be below $950,000 at the close of at least one of the next 45 trading days?
#
niter = 1.e5 # number of iterations
below = rep(0,niter) # set up storage

set.seed(2009)
for( i in 1:niter ){
  r = rnorm( 45, mean=0.05/253, sd=0.23/sqrt(253) )
  logPrice = log(1.e6) + cumsum(r) # the time series of portfolio values
  minlogP = min(logPrice)
  below[i] = as.numeric(minlogP < log(950000)) # equals 1 if the minimum price over 45 days is 
                                               # < 950K in ith simulation
}
# Probability the value of the stock falls below 950K at the los eof at least one of the next 45 trading days
mean(below) #proportion of sims where minimum price is less than 950K

#######################################################################################
# Simulation: Suppose the hedge fund will sell for a profit of at least $100,000
# if the value of the stock rises to at least $1,100,000 at the end of one of the first 
# 100 days, sell it for a loss if the value falls below $950,000 at the end of one 
# of the first 100 trading days, or sell after 100 trading days if the closing price has 
# stayed between $950,000 and $1,100,000
# 
# Problem 5: What is the probability that the hedge fund will make a profit of at least
# $100,000?
#
# Problem 6: What is the probability the hedge fund will suffer a loss?
#
# Problem 7: What is the expected profit from this trading strategy?
#

niter = 1.e5 # number of sims of price
n_days_simulation = 100
profit_target = 1.e6 + 100000.
log_profit_target = log(profit_target)
stop_loss = 1.e6 - 50000.
log_stop_loss = log(stop_loss)

above = rep(0,niter) # do we need to sell for profit during 100 days?
below = rep(0,niter) # do we need to sell for a loss during 100 days?
middle = rep(0,niter) # dont sell and hold 
profit = rep(0,niter)
strat_return = rep(0,niter) # greater than 100 days
set.seed (2009)

for(i in 1:niter){
  r = rnorm( n_days_simulation, mean=0.05/253, sd=0.23/sqrt(253) ) # trade for n simulated days 
  logPrice = log(1.e6) + cumsum(r) # the cumulative sum of portfolio values
  minlogP = min(logPrice)
  maxlogP = max(logPrice)
  
  # Are either of stoploss/profit targets triggered during this timeseries?: 
  #
  sl_triggered = minlogP <= log_stop_loss
  pt_triggered = maxlogP >= log_profit_target
  
  # only stoploss triggered in timeseries: 
  # 
  if( sl_triggered & !pt_triggered ){ 
    below[i] = 1
    profit[i] = -50000 # when value below 950K, incur loss of 50K
    days_in_trade = which(logPrice <= log_stop_loss)[1] 
  }
  
  # only profit target triggered in timeseries: 
  # 
  if( !sl_triggered & pt_triggered ){ 
    above[i] = 1
    profit[i] = +100000 # when value above 1,100,000, sell with profit of 100K
    days_in_trade = which(logPrice >= log_profit_target)[1] 
  }
  
  # if both values are triggered, which happened first?
  #
  if( sl_triggered & pt_triggered ){ 
    days_in_trade_to_min = which(logPrice <= log_stop_loss)[1] 
    days_in_trade_to_max = which(logPrice >= log_profit_target)[1] 
    
    if( days_in_trade_to_min < days_in_trade_to_max ){ # reach stoploss first: 
      below[i] = 1
      profit[i] = -50000
      days_in_trade = days_in_trade_to_min
    }else{  # reach profit target first: 
      above[i] = 1 
      profit[i] = +100000
      days_in_trade = days_in_trade_to_max
    }
  }
  
  # neither stoploss or profit target reached: 
  if( !sl_triggered & !pt_triggered ){
    middle[i] = 1
    profit[i] = exp(logPrice[n_days_simulation]) - 1.e6 # earn gain / below 1
    days_in_trade = n_days_simulation
  }
  
  # returns: 
  strat_return[i] = profit[i] / 50000.
  strat_return[i] = strat_return[i] / days_in_trade # convert to daily returns 
}
stopifnot( abs( mean(above)+mean(below)+mean(middle) - 1) < 1.e-6 )

#Probability the hedge fund will make a profit of at least $100K
print(mean(above))
#Probability the fund will suffer a loss
print(mean(below))
#Expected return from this trading strategy
print(mean(profit))
#Expected daily returns from this trading strategy
print(mean(strat_return))