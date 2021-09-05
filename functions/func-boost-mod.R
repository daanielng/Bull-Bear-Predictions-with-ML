
############################                     
# Boosting Trees using OOB #
############################

runbt=function(Y,indice,lag){

  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]  # lags of Y (predictors) corresponding to forecast horizon 

  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]   #retrieve the last  observations if one-step forecast
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)]  #last observations: y_T,y_t-1...y_t-h
  }
  
  df=as.data.frame(rbind(X,X.out))
  
  model=gbm(y~.,data=df[1:nrow(X),],distribution='bernoulli',bag.fraction = .5,
          interaction.depth=5,n.trees=10000,shrinkage=.01)
  #model = gbm(y~.,data=df[1:nrow(X),],distribution='bernoulli',bag.fraction = .5,
  #               interaction.depth=5,n.trees=10000,shrinkage=.01,cv.folds=10)
  
  bestoob=gbm.perf(model, method="OOB")
  #bestcv=gbm.perf(model, method="cv")

  pred = predict(model, newdata = df[nrow(X)+1,], n.trees = bestoob, type = "response")
  #pred = predict(model, newdata = df[nrow(X)+1,], n.trees = bestcv, type = "response")
  
  importance=summary(model,n.trees=bestoob,plotit= FALSE)

  return(list("model"=model,"pred"=pred, "importance"=importance)) #return the estimated model and h-step forecast
}

bt.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  # save.importance=list() #blank for saving variable importance
  save.importance=list()
  save.pred=matrix(NA,nprev,1) ##blank for forecasts
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    boost=runbt(Y.window,indice,lag)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+nprev-i),]=boost$pred #save the forecast
    #browser()
    save.importance[[i]]=boost$importance #save variable importance
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice]#get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors, "save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}


###########################                     
# Boosting Trees using CV #
###########################

runbt2=function(Y,indice,lag){           #CV
  
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]  # lags of Y (predictors) corresponding to forecast horizon 
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]   #retrieve the last  observations if one-step forecast
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)]  #last observations: y_T,y_t-1...y_t-h
  }
  
  df=as.data.frame(rbind(X,X.out))
  
  #model=gbm(y~.,data=df[1:nrow(X),],distribution='bernoulli',bag.fraction = .5,
  #        interaction.depth=5,n.trees=10000,shrinkage=.01)
  model = gbm(y~.,data=df[1:nrow(X),],distribution='bernoulli',bag.fraction = .5,
              interaction.depth=5,n.trees=10000,shrinkage=.01,cv.folds=10)
  
  #bestoob=gbm.perf(model, method="OOB")
  bestcv=gbm.perf(model, method="cv")
  
  #pred = predict(model, newdata = df[nrow(X)+1,], n.trees = bestoob, type = "response")
  pred = predict(model, newdata = df[nrow(X)+1,], n.trees = bestcv, type = "response")
  
  importance=summary(model,n.trees=bestcv,plotit=FALSE)
  
  return(list("model"=model,"pred"=pred, "importance"=importance)) #return the estimated model and h-step forecast
}


bt.rolling.window2=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,nprev,1) ##blank for forecasts
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    boost=runbt2(Y.window,indice,lag)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+nprev-i),]=boost$pred #save the forecast
    #browser()
    save.importance[[i]]=boost$importance #save variable importance
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice]#get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"errors"=errors, "save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}

