# This code simulates a series of naive trades of an initial $100000
# $50000 is invested at the start
# When the market turns from Bear to Bull, another $50000 is invested
# When the market turns from Bull to Bear, $50000 is taken out


rm(list=ls())

setwd("~/Y4S2/EC4308/Project")

# Load predictions
load("~/Y4S2/EC4308/Project/compiled_pred.RDa")

# Load SP500 data
sp500.raw = read.csv("SP500.csv")
sp500.raw$sasdate = as.Date(sp500.raw$sasdate, "%d/%m/%Y")
sp500 = sp500.raw[481:720,]$S.P.500

# Helper function
# Rounds up prediction to 1 (Bull) or 0 (Bear)
bb_class = function(val, threshold = 0.5) {
  if (val >= threshold) {
    return(1)
  } else {
    return(0)
  }
}

# Helper function
# Increments portfolio value between old and new period
sp_increment = function(val, old.sp, new.sp) {
  return(val/old.sp*new.sp) # Divide portfolio by old index then multiply by new index
}

# Calculates portfolio value given a list of bull/bear predictions and S&P 500 index
# pred.list and sp.list should be of the same time frame and same length
predict_portfolio = function(pred.list, sp.list, initial.state = 0, initial.value = 100000, buy.amount = 5000) {
  old.sp = sp.list[1]
  current.sp = sp.list[1]
  current.state = initial.state
  current.portfolio = initial.value
  
  # Loop through the list of predictions
  for (i in 1:length(pred.list)) {
    bb_pred = bb_class(pred.list[i])  # Round up to 1 (Bull) or down to 0 (Bear)
    current.sp = sp.list[i] # Get current S&P500 value
    current.portfolio = sp_increment(current.portfolio, old.sp, current.sp) # Adjust value of current portfolio based on past and current S&P500 value
    
    #Bull Signal
    if (bb_pred > current.state) {
      current.portfolio = current.portfolio + buy.amount # Increase portfolio value by buy.amount
      current.state = 1 # Update state
    } 
    
    #Bear Signal
    else if (bb_pred < current.state) {
      current.portfolio = current.portfolio - buy.amount # Decrease portfolio balue by buy.amount
      current.state = 0 # Update state
    }
    old.sp = current.sp # Update S&P state
  }
  return(current.portfolio)
}


# Run predictions
preds = list(bt1o, bt3o, bt6o, bt12o, dp1, dp3, dp6, dp12, rf1c, rf3c, rf6c, rf12c, rll1, rll3, rll6, rll12)
prednames = c('bt1o', 'bt3o', 'bt6o', 'bt12o', 'dp1', 'dp3', 'dp6', 'dp12', 'rf1c', 'rf3c', 'rf6c', 'rf12c', 'rll1', 'rll3', 'rll6', 'rll12')
preds.matrix = matrix(prednames)
preds.matrix = cbind(preds.matrix, 0)
colnames(preds.matrix) = c('Model', 'Predicted Portfolio')

for (i in 1:length(preds)) {
  sp.pred = predict_portfolio(preds[[i]]$pred, sp500,  initial.state = 0, initial.value = 50000, buy.amount = 50000)
  preds.matrix[i,2] = sp.pred
}


bb.actual = Y$bb[481:720]
bb.portfolio = predict_portfolio(bb.actual, sp500, initial.state = 0, initial.value = 50000, buy.amount = 50000)

# Trading a $100k portfolio based on bull/bear predictions
preds.matrix

# Trading a $100k portfolio based on actuall bull/bear value
bb.portfolio


