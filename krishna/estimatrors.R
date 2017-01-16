n <- length(compactness)
gcvPMSE <- function (x, y, h){
  model <- locpolreg(x,y,h,doing.plot = F)
  traceS <- sum(diag(model$S))
  mhat <- model$mtgr
  return(sum((y - mhat)^2)*n/(n-traceS)^2)
}

sqrt(gcvPMSE(compactness, width, 3.1964838))

estimators(compactness,width)
estimators <- function(x,y){
  data = cbind(x,y)
  if(is.unsorted(x, na.rm = FALSE, strictly = FALSE)){
    data <- data[order(x),]
  }
  
  #Proposal of Rice
  sum_sq = 0
  for(i in 2:nrow(data)){
    sum_sq = sum_sq + (data[i,2]-data[i-1,2])^2
  }
  Rice_var_estimator = unname(1/(2*(nrow(data)-1)) * sum_sq)
  
  #Proposal of Gasser Sroka and Jennen Steinmetz
  
  sum_sq = 0
  # Build the estimator 
  for(i in 2:(nrow(data)-1)){
    if(data[i-1,1] == data[i,1] && data[i+1,1] == data[i,1] ){
      xminus = max(data[data[,1]<data[i,1],1])
      xplus =  min(data[data[,1]>data[i,1],1])
      a = (xplus-data[i,1])/(xplus-xminus)
      b = (data[i,1]-xminus)/(xplus-xminus)
    } else{
      a = (data[i+1,1]-data[i,1])/(data[i+1,1]-data[i-1,1])
      b = (data[i,1]-data[i-1,1])/(data[i+1,1]-data[i-1,1])
    }
    y_est = a*data[i-1,2] + b*data[i+1,2]
    error_sq = (y_est - data[i,2])^2
    sum_sq = sum_sq + 1/(a^2+b^2+1)*error_sq
  }
  Interpolation_var_estimator = unname(sum_sq/(nrow(data)-2))
  return(c("Rice Variance Estimator" = Rice_var_estimator,"Interpolation Variance Estimator" = Interpolation_var_estimator))
}


