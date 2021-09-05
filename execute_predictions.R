rm(list=ls())

RMSE <- function(pred, truth){
  return(sqrt(mean((truth - pred)^2)))
}

setwd("~/Y4S2/EC4308/Project")

tfredmd = read.csv("TSdata.csv")
tfredmd$sasdate = as.Date(tfredmd$sasdate, "%m/%d/%Y")
rownames(tfredmd) = tfredmd$sasdate

str(tfredmd)

Y = tfredmd[-1:-12,-1]
yy = Y[,4]

nprev = 240

oosy = tail(yy,nprev)

Ymat = as.matrix(Y)


#########################
# Random Walk Forecasts #
#########################

#create lags
rwtemp=embed(yy,13)
#Simple RW forecast:
rw1c=tail(rwtemp[,2],nprev)
rw3c=tail(rwtemp[,4],nprev)
rw6c=tail(rwtemp[,7],nprev)
rw12c=tail(rwtemp[,13],nprev)

#Collect RMSE's for randomw walk:
rw.rmse1=RMSE(oosy,rw1c)
rw.rmse3=RMSE(oosy,rw3c)
rw.rmse6=RMSE(oosy,rw6c)
rw.rmse12=RMSE(oosy,rw12c)



###################
# AR(p) Forecasts #
###################

source("func-ar.R")
bar1c=ar.rolling.window(Y,nprev,5,1,type="fixed") #1-step AR forecast
bar3c=ar.rolling.window(Y,nprev,5,3,type="fixed") #3-step AR forecast
bar6c=ar.rolling.window(Y,nprev,5,6,type="fixed") #6-step AR forecast
bar12c=ar.rolling.window(Y,nprev,5,12,type="fixed") #12-step AR forecast



############################
# Random Forests Forecasts #
############################

source("func-rf.R")

# Use Ymat instead of Y as function only takes matrices

rf1c=rf.rolling.window(Ymat,nprev,4,1)
rf3c=rf.rolling.window(Ymat,nprev,4,3)
rf6c=rf.rolling.window(Ymat,nprev,4,6)
rf12c=rf.rolling.window(Ymat,nprev,4,12)

#See the RMSE:
rf.rmse1=rf1c$errors[1]
rf.rmse3=rf3c$errors[1]
rf.rmse6=rf6c$errors[1]
rf.rmse12=rf12c$errors[1]
