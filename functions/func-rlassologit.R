#Here we follow Medeiros et al. (2019) in defining four functions:

#One for forming forecasts using LASSO or Elastic Net model selected on BIC, which will be called
#on each iteration of the rolling window forecasting exercise.

#The second one for producing the series of h-step LASSO forecasts using rolling window.

#The third one will take the resulting coefficients from LASSO, seek out the nonzero ones and rerun OLS with selected predictors to produce post-LASSO forecast.
#The fourth one will call on the third one for producing the series of h-step post-LASSO forecasts using rolling window.

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#5) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"
runrlassologit=function(Y,indice,lag){
  
  #dum=Y[,ncol(Y)] # extract dummy from data
  #Y=Y[,-ncol(Y)] #data without the dummy
  comp=princomp(scale(Y,scale=FALSE)) # compute principal components to add as predictors
  Y2=cbind(Y,comp$scores[,1:4]) #augment predictors by the first 4 principal components
  aux=embed(Y2,4+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=aux[,indice] #  Y variable aligned/adjusted for missing data due do lags
  X=aux[,-c(1:(ncol(Y2)*lag))]   # lags of Y (predictors) corresponding to forecast horizon   
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)] #retrieve the last  observations if one-step forecast  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }
  #dum=tail(dum,length(y)) #cut the dummy to size to account for lost observations due to lags
  
  
  #Here we replace the glmnet-based LASSO by rlasso:
  
  #If we want to use predict() with rlasso, we need to stuff things into a dataframe
  #type object first:
  
  #All the predictors:
  #tempd=cbind(X,dum)
  #Append the test set at the end and set as dataframe:
  df=as.data.frame(rbind(X,X.out))
  
  #Run post-lasso (default setting, heteroskedasticity-adjusted):
  model=rlassologit(df[1:nrow(X),],y) 
  
  
  pred=predict(model,newdata=df[nrow(X)+1,]) #generate the forecast (note that c(X.out,0) is the last row of the dataframe)
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


#This function will repeatedly call the previous function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon


#5) alpha - the alpha parameter determining whether we use LASSO (alpha=1) or ElNet (alpha=0.5)

#6) IC - information criterion used to select lambda. Can set to: "bic", "aic" or "aicc"


rlassologit.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.coef=matrix(NA,nprev,21-4+ncol(Y)*4 ) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){ #NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    rlassologit=runrlassologit(Y.window,indice,lag) #call the modified function to fit rlasso and predict
    save.coef[(1+nprev-i),]=rlassologit$model$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=rlassologit$pred #save the forecast
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


#We don't need the rests of the function (we already did post-lasso).
#You can actually specify a TRUE/FALSE input and feed it to rlasso in order to make this fucntion
#usable for both the usual and post-LASSO with theoretical plug-in.