rm(list=ls())

load("compiled_pred.Rda")

truebb=Y[481:720,22]
vtruebb=Y[481:600,22]


#################################
# Computing Lift and ROC curves #
#################################

#Number of points on the threshold grid
ns=1000

#Create sequence of thresholds
sv = seq(from=.0,to=1,length.out=ns)

#Blanks to store relevant quantities:
FPdp1 = rep(0,ns)
FNdp1 = rep(0,ns)
TPdp1 = rep(0,ns)
Ndp1 = rep(0,ns)
FPdp3 = rep(0,ns)
FNdp3 = rep(0,ns)
TPdp3 = rep(0,ns)
Ndp3 = rep(0,ns)
FPdp6 = rep(0,ns)
FNdp6 = rep(0,ns)
TPdp6 = rep(0,ns)
Ndp6 = rep(0,ns)
FPdp12 = rep(0,ns)
FNdp12 = rep(0,ns)
TPdp12 = rep(0,ns)
Ndp12 = rep(0,ns)

FPrll1 = rep(0,ns)
FNrll1 = rep(0,ns)
TPrll1 = rep(0,ns)
Nrll1 = rep(0,ns)
FPrll3 = rep(0,ns)
FNrll3 = rep(0,ns)
TPrll3 = rep(0,ns)
Nrll3 = rep(0,ns)
FPrll6 = rep(0,ns)
FNrll6 = rep(0,ns)
TPrll6 = rep(0,ns)
Nrll6 = rep(0,ns)
FPrll12 = rep(0,ns)
FNrll12 = rep(0,ns)
TPrll12 = rep(0,ns)
Nrll12 = rep(0,ns)

FPrf1c = rep(0,ns)
FNrf1c = rep(0,ns)
TPrf1c = rep(0,ns)
Nrf1c = rep(0,ns)
FPrf3c = rep(0,ns)
FNrf3c = rep(0,ns)
TPrf3c = rep(0,ns)
Nrf3c = rep(0,ns)
FPrf6c = rep(0,ns)
FNrf6c = rep(0,ns)
TPrf6c = rep(0,ns)
Nrf6c = rep(0,ns)
FPrf12c = rep(0,ns)
FNrf12c = rep(0,ns)
TPrf12c = rep(0,ns)
Nrf12c = rep(0,ns)

FPbt1o = rep(0,ns)
FNbt1o = rep(0,ns)
TPbt1o = rep(0,ns)
Nbt1o = rep(0,ns)
FPbt3o = rep(0,ns)
FNbt3o = rep(0,ns)
TPbt3o = rep(0,ns)
Nbt3o = rep(0,ns)
FPbt6o = rep(0,ns)
FNbt6o = rep(0,ns)
TPbt6o = rep(0,ns)
Nbt6o = rep(0,ns)
FPbt12o = rep(0,ns)
FNbt12o = rep(0,ns)
TPbt12o = rep(0,ns)
Nbt12o = rep(0,ns)

FPbt1c = rep(0,ns)
FNbt1c = rep(0,ns)
TPbt1c = rep(0,ns)
Nbt1c = rep(0,ns)
FPbt3c = rep(0,ns)
FNbt3c = rep(0,ns)
TPbt3c = rep(0,ns)
Nbt3c = rep(0,ns)
FPbt6c = rep(0,ns)
FNbt6c = rep(0,ns)
TPbt6c = rep(0,ns)
Nbt6c = rep(0,ns)
FPbt12c = rep(0,ns)
FNbt12c = rep(0,ns)
TPbt12c = rep(0,ns)
Nbt12c = rep(0,ns)


#Here we compute the N, true positive rate and false positive rates for the tree fits and logit:

for(i in 1:ns) {
  Ndp1[i] = sum(dp1$pred[,1]>sv[i])/length(truebb)
  TPdp1[i] = sum((dp1$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPdp1[i] = sum((dp1$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNdp1[i] = sum((dp1$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Ndp3[i] = sum(dp3$pred[,1]>sv[i])/length(truebb)
  TPdp3[i] = sum((dp3$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPdp3[i] = sum((dp3$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNdp3[i] = sum((dp3$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Ndp6[i] = sum(dp6$pred[,1]>sv[i])/length(truebb)
  TPdp6[i] = sum((dp6$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPdp6[i] = sum((dp6$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNdp6[i] = sum((dp6$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Ndp12[i] = sum(dp12$pred[,1]>sv[i])/length(truebb)
  TPdp12[i] = sum((dp12$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPdp12[i] = sum((dp12$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNdp12[i] = sum((dp12$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  
  Nrll1[i] = sum(rll1$pred[,1]>sv[i])/length(truebb)
  TPrll1[i] = sum((rll1$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrll1[i] = sum((rll1$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrll1[i] = sum((rll1$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrll3[i] = sum(rll3$pred[,1]>sv[i])/length(truebb)
  TPrll3[i] = sum((rll3$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrll3[i] = sum((rll3$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrll3[i] = sum((rll3$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrll6[i] = sum(rll6$pred[,1]>sv[i])/length(truebb)
  TPrll6[i] = sum((rll6$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrll6[i] = sum((rll6$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrll6[i] = sum((rll6$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrll12[i] = sum(rll12$pred[,1]>sv[i])/length(truebb)
  TPrll12[i] = sum((rll12$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrll12[i] = sum((rll12$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrll12[i] = sum((rll12$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  
  Nrf1c[i] = sum(rf1c$pred[,1]>sv[i])/length(truebb)
  TPrf1c[i] = sum((rf1c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrf1c[i] = sum((rf1c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrf1c[i] = sum((rf1c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrf3c[i] = sum(rf3c$pred[,1]>sv[i])/length(truebb)
  TPrf3c[i] = sum((rf3c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrf3c[i] = sum((rf3c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrf3c[i] = sum((rf3c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrf6c[i] = sum(rf6c$pred[,1]>sv[i])/length(truebb)
  TPrf6c[i] = sum((rf6c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrf6c[i] = sum((rf6c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrf6c[i] = sum((rf6c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nrf12c[i] = sum(rf12c$pred[,1]>sv[i])/length(truebb)
  TPrf12c[i] = sum((rf12c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPrf12c[i] = sum((rf12c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNrf12c[i] = sum((rf12c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  
  Nbt1o[i] = sum(bt1o$pred[,1]>sv[i])/length(truebb)
  TPbt1o[i] = sum((bt1o$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt1o[i] = sum((bt1o$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt1o[i] = sum((bt1o$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt3o[i] = sum(bt3o$pred[,1]>sv[i])/length(truebb)
  TPbt3o[i] = sum((bt3o$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt3o[i] = sum((bt3o$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt3o[i] = sum((bt3o$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt6o[i] = sum(bt6o$pred[,1]>sv[i])/length(truebb)
  TPbt6o[i] = sum((bt6o$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt6o[i] = sum((bt6o$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt6o[i] = sum((bt6o$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt12o[i] = sum(bt12o$pred[,1]>sv[i])/length(truebb)
  TPbt12o[i] = sum((bt12o$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt12o[i] = sum((bt12o$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt12o[i] = sum((bt12o$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  
  Nbt1c[i] = sum(bt1c$pred[,1]>sv[i])/length(truebb)
  TPbt1c[i] = sum((bt1c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt1c[i] = sum((bt1c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt1c[i] = sum((bt1c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt3c[i] = sum(bt3c$pred[,1]>sv[i])/length(truebb)
  TPbt3c[i] = sum((bt3c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt3c[i] = sum((bt3c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt3c[i] = sum((bt3c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt6c[i] = sum(bt6c$pred[,1]>sv[i])/length(truebb)
  TPbt6c[i] = sum((bt6c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt6c[i] = sum((bt6c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt6c[i] = sum((bt6c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
  Nbt12c[i] = sum(bt12c$pred[,1]>sv[i])/length(truebb)
  TPbt12c[i] = sum((bt12c$pred[,1]>sv[i]) & (truebb==1))/sum(truebb==1)
  FPbt12c[i] = sum((bt12c$pred[,1]>sv[i]) & (truebb==0))/sum(truebb==0)
  FNbt12c[i] = sum((bt12c$pred[,1]<sv[i]) & (truebb==1))/sum(truebb==1)
}


#Plot the relationships between of the relevant quantities with s:

par(mfrow=c(1,3))
par(mai=c(0.9,0.9,0.4,0.4))

plot(sv,Ndp1,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPdp1,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPdp1,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Ndp3,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPdp3,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPdp3,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Ndp6,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPdp6,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPdp6,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Ndp12,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPdp12,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPdp12,xlab='s',type='l',col='blue',cex.lab=2.0)



plot(sv,Nrll1,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrll1,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrll1,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrll3,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrll3,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrll3,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrll6,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrll6,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrll6,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrll12,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrll12,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrll12,xlab='s',type='l',col='blue',cex.lab=2.0)



plot(sv,Nrf1c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrf1c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrf1c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrf3c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrf3c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrf3c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrf6c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrf6c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrf6c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nrf12c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPrf12c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPrf12c,xlab='s',type='l',col='blue',cex.lab=2.0)



plot(sv,Nbt1o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt1o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt1o,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt3o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt3o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt3o,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt6o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt6o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt6o,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt12o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt12o,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt12o,xlab='s',type='l',col='blue',cex.lab=2.0)



plot(sv,Nbt1c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt1c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt1c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt3c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt3c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt3c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt6c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt6c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt6c,xlab='s',type='l',col='blue',cex.lab=2.0)

plot(sv,Nbt12c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,TPbt12c,xlab='s',type='l',col='blue',cex.lab=2.0)
plot(sv,FPbt12c,xlab='s',type='l',col='blue',cex.lab=2.0)


# Plot the ROC and Lift curves 

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPdp1,TPdp1,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-dp1',cex.main=2)
plot(Ndp1,TPdp1,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-dp1',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPdp3,TPdp3,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-dp3',cex.main=2)
plot(Ndp3,TPdp3,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-dp3',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPdp6,TPdp6,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-dp6',cex.main=2)
plot(Ndp6,TPdp6,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-dp6',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPdp12,TPdp12,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-dp12',cex.main=2)
plot(Ndp12,TPdp12,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-dp12',cex.main=2)



par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrll1,TPrll1,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rll1',cex.main=2)
plot(Nrll1,TPrll1,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rll1',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrll3,TPrll3,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rll3',cex.main=2)
plot(Nrll3,TPrll3,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rll3',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrll6,TPrll6,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rll6',cex.main=2)
plot(Nrll6,TPrll6,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rll6',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrll12,TPrll12,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rll12',cex.main=2)
plot(Nrll12,TPrll12,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rll12',cex.main=2)



par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrf1c,TPrf1c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rf1c',cex.main=2)
plot(Nrf1c,TPrf1c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rf1c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrf3c,TPrf3c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rf3c',cex.main=2)
plot(Nrf3c,TPrf3c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rf3c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrf6c,TPrf6c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rf6c',cex.main=2)
plot(Nrf6c,TPrf6c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rf6c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPrf12c,TPrf12c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-rf12c',cex.main=2)
plot(Nrf12c,TPrf12c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-rf12c',cex.main=2)



par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt1o,TPbt1o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt1o',cex.main=2)
plot(Nbt1o,TPbt1o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt1o',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt3o,TPbt3o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt3o',cex.main=2)
plot(Nbt3o,TPbt3o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt3o',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt6o,TPbt6o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt6o',cex.main=2)
plot(Nbt6o,TPbt6o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt6o',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt12o,TPbt12o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt12o',cex.main=2)
plot(Nbt12o,TPbt12o,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt12o',cex.main=2)




par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt1c,TPbt1c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt1c',cex.main=2)
plot(Nbt1c,TPbt1c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt1c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt3c,TPbt3c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt3c',cex.main=2)
plot(Nbt3c,TPbt3c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt3c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt6c,TPbt6c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt6c',cex.main=2)
plot(Nbt6c,TPbt6c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt6c',cex.main=2)

par(mai=c(0.9,0.9,0.4,0.4))
par(mfrow=c(1,2))
plot(FPbt12c,TPbt12c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'FP')
abline(0,1,lty=2)
title(main='ROC-bt12c',cex.main=2)
plot(Nbt12c,TPbt12c,type='l',col='blue',cex.lab=2.0, ylab = 'TP', xlab = 'N')
abline(0,1,lty=2)
title(main='Lift-bt12c',cex.main=2)






# Combined lift 1-step

par(mfrow=c(1,1))
par(mai=c(0.9,0.9,0.4,0.4))

plot(Ndp1,TPdp1,type='l',col = 'black', main = "Lift 1-step", xlab = "N", ylab = "TP")
lines(Nrll1,TPrll1,type='l', col = 'blue')
lines(Nbt1o,TPbt1o,type='l', col = 'red')
lines(Nbt1c,TPbt1c,type='l', col = 'green')
lines(Nrf1c,TPbt1c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined lift 3-step

plot(Ndp3,TPdp3,type='l',col = 'black', main = "Lift 3-step", xlab = "N", ylab = "TP")
lines(Nrll3,TPrll3,type='l', col = 'blue')
lines(Nbt3o,TPbt3o,type='l', col = 'red')
lines(Nbt3c,TPbt3c,type='l', col = 'green')
lines(Nrf3c,TPbt3c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined lift 6-step

plot(Ndp6,TPdp6,type='l',col = 'black', main = "Lift 6-step", xlab = "N", ylab = "TP")
lines(Nrll6,TPrll6,type='l', col = 'blue')
lines(Nbt6o,TPbt6o,type='l', col = 'red')
lines(Nbt6c,TPbt6c,type='l', col = 'green')
lines(Nrf6c,TPbt6c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined lift 12-step

plot(Ndp12,TPdp12,type='l',col = 'black', main = "Lift 12-step", xlab = "N", ylab = "TP")
lines(Nrll12,TPrll12,type='l', col = 'blue')
lines(Nbt12o,TPbt12o,type='l', col = 'red')
lines(Nbt12c,TPbt12c,type='l', col = 'green')
lines(Nrf12c,TPbt12c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))



# Combined roc 1-step

par(mfrow=c(1,1))
par(mai=c(0.9,0.9,0.4,0.4))

plot(FPdp1,TPdp1,type='l',col = 'black', main = "ROC 1-step", xlab = "FP", ylab = "TP")
lines(FPrll1,TPrll1,type='l', col = 'blue')
lines(FPbt1o,TPbt1o,type='l', col = 'red')
lines(FPbt1c,TPbt1c,type='l', col = 'green')
lines(FPrf1c,TPbt1c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined roc 3-step

plot(FPdp3,TPdp3,type='l',col = 'black', main = "ROC 3-step", xlab = "FP", ylab = "TP")
lines(FPrll3,TPrll3,type='l', col = 'blue')
lines(FPbt3o,TPbt3o,type='l', col = 'red')
lines(FPbt3c,TPbt3c,type='l', col = 'green')
lines(FPrf3c,TPbt3c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined roc 6-step

plot(FPdp6,TPdp6,type='l',col = 'black', main = "ROC 6-step", xlab = "FP", ylab = "TP")
lines(FPrll6,TPrll6,type='l', col = 'blue')
lines(FPbt6o,TPbt6o,type='l', col = 'red')
lines(FPbt6c,TPbt6c,type='l', col = 'green')
lines(FPrf6c,TPbt6c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Combined roc 12-step

plot(FPdp12,TPdp12,type='l',col = 'black', main = "ROC 12-step", xlab = "FP", ylab = "TP")
lines(FPrll12,TPrll12,type='l', col = 'blue')
lines(FPbt12o,TPbt12o,type='l', col = 'red')
lines(FPbt12c,TPbt12c,type='l', col = 'green')
lines(FPrf12c,TPbt12c,type='l', col = 'pink')
abline(0,1,lty=2) #45-degree line
legend("bottomright", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Precision-Recall combined 1step

plot(TPdp1/(TPdp1+FNdp1),TPdp1/(TPdp1+FPdp1),type='l',col = 'black', main = "Precision-Recall 1-step", xlab = "Recall", ylab = "Precision")
lines(TPrll1/(TPrll1+FNrll1),TPrll1/(TPrll1+FPrll1),type='l', col = 'blue')
lines(TPbt1o/(TPbt1o+FNbt1o),TPbt1o/(TPbt1o+FPbt1o),type='l', col = 'red')
lines(TPbt1c/(TPbt1c+FNbt1c),TPbt1c/(TPbt1c+FPbt1c),type='l', col = 'green')
lines(TPrf1c/(TPrf1c+FNrf1c),TPrf1c/(TPrf1c+FPrf1c),type='l', col = 'pink')
legend("bottom", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Precision-Recall combined 3step

plot(TPdp3/(TPdp3+FNdp3),TPdp3/(TPdp3+FPdp3),type='l',col = 'black', main = "Precision-Recall 3-step", xlab = "Recall", ylab = "Precision")
lines(TPrll3/(TPrll3+FNrll3),TPrll3/(TPrll3+FPrll3),type='l', col = 'blue')
lines(TPbt3o/(TPbt3o+FNbt3o),TPbt3o/(TPbt3o+FPbt3o),type='l', col = 'red')
lines(TPbt3c/(TPbt3c+FNbt3c),TPbt3c/(TPbt3c+FPbt3c),type='l', col = 'green')
lines(TPrf3c/(TPrf3c+FNrf3c),TPrf3c/(TPrf3c+FPrf3c),type='l', col = 'pink')
legend("bottom", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))


# Precision-Recall combined 6step

plot(TPdp6/(TPdp6+FNdp6),TPdp6/(TPdp6+FPdp6),type='l',col = 'black', main = "Precision-Recall 6-step", xlab = "Recall", ylab = "Precision") 
lines(TPrll6/(TPrll6+FNrll6),TPrll6/(TPrll6+FPrll6),type='l', col = 'blue')
lines(TPbt6o/(TPbt6o+FNbt6o),TPbt6o/(TPbt6o+FPbt6o),type='l', col = 'red')
lines(TPbt6c/(TPbt6c+FNbt6c),TPbt6c/(TPbt6c+FPbt6c),type='l', col = 'green')
lines(TPrf6c/(TPrf6c+FNrf6c),TPrf6c/(TPrf6c+FPrf6c),type='l', col = 'pink')
legend("bottom", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))

# Precision-Recall combined 12step

plot(TPdp12/(TPdp12+FNdp12),TPdp12/(TPdp12+FPdp12),type='l',col = 'black', main = "Precision-Recall 12-step", xlab = "Recall", ylab = "Precision")
lines(TPrll12/(TPrll12+FNrll12),TPrll12/(TPrll12+FPrll12),type='l', col = 'blue')
lines(TPbt12o/(TPbt12o+FNbt12o),TPbt12o/(TPbt12o+FPbt12o),type='l', col = 'red')
lines(TPbt12c/(TPbt12c+FNbt12c),TPbt12c/(TPbt12c+FPbt12c),type='l', col = 'green')
lines(TPrf12c/(TPrf12c+FNrf12c),TPrf12c/(TPrf12c+FPrf12c),type='l', col = 'pink')
legend("bottom", c("Dynamic Probit","LASSO-logit","Boosting-OOB","Boosting-CV","Bagging"),
       lty=c(1,1,1,1,1) ,col=c("black","blue","red","green","pink"))

