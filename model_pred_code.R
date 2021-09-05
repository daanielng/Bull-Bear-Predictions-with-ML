rm(list=ls())

load(file='y_bb.Rdata')

#Auxiliary function to compute root MSE
RMSE <- function(pred, truth){ #start and end body of the function by { } - same as a loop 
  return(sqrt(mean((truth - pred)^2)))
}

yy = Y$bb
test = Y[601:720,]
nprev = 240
ntrain = 480
oosy = tail(yy,nprev)
Ymat = as.matrix(Y)


## Dynamic Probit - Baseline Model
source('func-dp.R')

dp1=dp.rolling.window(Ymat,nprev,22,1)
dp3=dp.rolling.window(Ymat,nprev,22,3)
dp6=dp.rolling.window(Ymat,nprev,22,6)
dp12=dp.rolling.window(Ymat,nprev,22,12)

dp.rmse1=dp1$errors[1]
dp.rmse3=dp3$errors[1]
dp.rmse6=dp6$errors[1]
dp.rmse12=dp12$errors[1]



## Lasso Logit
library(hdm)
library(glmnet)
source("func-rlassologit.R")

rll1=rlassologit.rolling.window(Ymat,nprev,indice=22,lag=1)
rll3=rlassologit.rolling.window(Ymat,nprev,indice=22,lag=3)
rll6=rlassologit.rolling.window(Ymat,nprev,indice=22,lag=6)
rll12=rlassologit.rolling.window(Ymat,nprev,indice=22,lag=12)

rll.rmse1=rll1$errors[1]
rll.rmse3=rll3$errors[1]
rll.rmse6=rll6$errors[1]
rll.rmse12=rll12$errors[1]


## Random Forests
library(randomForest)
source("func-rf-mod.R")

rftune = tuneRF(Ymat, Y$bb, mtryStart=floor(sqrt(ncol(Ymat))), stepFactor=2, improve=0.05, nodesize=5, ntree=5000, doBest=TRUE, plot=FALSE, trace=FALSE)
rftune$mtry #tuned number of predictors:22
rft.pred = predict(rftune, newdata=test)
RMSE(rft.pred,test$bb)

rf1c=rf.rolling.window(Ymat,nprev,22,1)
rf3c=rf.rolling.window(Ymat,nprev,22,3)
rf6c=rf.rolling.window(Ymat,nprev,22,6)
rf12c=rf.rolling.window(Ymat,nprev,22,12)

rf.rmse1=rf1c$errors[1]
rf.rmse3=rf3c$errors[1]
rf.rmse6=rf6c$errors[1]
rf.rmse12=rf12c$errors[1]


## Boosted Trees
library(gbm)
source("func-boost-mod.R")

# With OOB
bt1o=bt.rolling.window(Ymat,nprev,22,1)
bt3o=bt.rolling.window(Ymat,nprev,22,3)
bt6o=bt.rolling.window(Ymat,nprev,22,6)
bt12o=bt.rolling.window(Ymat,nprev,22,12)

bt.rmse1o=bt1o$errors[1]
bt.rmse3o=bt3o$errors[1]
bt.rmse6o=bt6o$errors[1]
bt.rmse12o=bt12o$errors[1]

# With CV
bt1c=bt.rolling.window2(Ymat,nprev,22,1)
bt3c=bt.rolling.window2(Ymat,nprev,22,3)
bt6c=bt.rolling.window2(Ymat,nprev,22,6)
bt12c=bt.rolling.window2(Ymat,nprev,22,12)

bt.rmse1c=bt1c$errors[1]
bt.rmse3c=bt3c$errors[1]
bt.rmse6c=bt6c$errors[1]
bt.rmse12c=bt12c$errors[1]

save(Y, Ymat, dp1, dp3, dp6, dp12, rll1, rll3, rll6, rll12, rf1c, rf3c, rf6c, rf12c, bt1o, bt3o, bt6o, bt12o, bt1c, bt3c, bt6c, bt12c, file="compiled_pred.Rda")
