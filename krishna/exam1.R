rm(list=ls())

# SM 
# Local poly Regression
# LOESS
# loess and non-parametric regression

setwd('/Users/krishna/Downloads/asm_exam/3rd_batch')
source("locpolreg.R")
load('seeds.Rdata')
set.seed(123)

attach(seeds)
plot(seeds$width)
# Use bandwidth LOCPOLREG Param X and then Y
h.v = exp(seq(from=log(.5), to = log(15), length=12))

kfold <- function(data, k = 5, h){
  data <- data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  avg.fold <- c()
  for(i in 1:k){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test <- data[testIndexes, ]
    train <- data[-testIndexes, ]
    error <- model(train,test, h)
    avg.fold <- c(avg.fold, error)
  }
  return(mean(avg.fold))
}

model <- function(train,test,h){
  model1 <- locpolreg(train$compactness,train$width, h, 
                      tg=test$compactness, doing.plot = F)
  yhat <- model1$mtgr
  sqrd_error <- (test$width - yhat)^2
  return(mean(sqrd_error))
}

eval.matrix <- matrix(nrow=length(h.v),ncol = 2)
for(i in 1:length(h.v)){
  errors <- kfold(seeds, k =  10, h =h.v[i])
  eval.matrix[i,] <- c(h.v[i],errors)
}

which.min(eval.matrix[,2])

# Step 1 : Fit
model3 <- locpolreg(seeds$compactness,seeds$width, h=3.1964838)

# Step 2 : Estimated Residuals 
mhat <- model3$mtgr
residuals <- (seeds$width - model3$mtgr)
z <- log(residuals^2)

# Step 3: nonparametric regression to data
model4 <- locpolreg(seeds$compactness, z, h=3.1964838)
qhat <- model4$mtgr
l1 <- mhat + 1.96*sqrt(exp(qhat))
l2 <- mhat - 1.96*sqrt(exp(qhat))

order <- order(seeds$compactness)

# Scatter Plot
plot(compactness, width)
lines(compactness[order], model3$mtgr[order], col='red')
lines(compactness[order], l1[order], col="red", lwd=3)
lines(compactness[order], l2[order], col="purple", lwd=3)

# Rice - s
# See Estimators
estimators <- sqrt(estimators(df$LSTAT,df$ROOM))
estimators <- sqrt(0.05321601)

# 2 b 
n <- length(width)
gcvPMSE <- function (x, y, h){
  model <- locpolreg(x,y,h,doing.plot = F)
  traceS <- sum(diag(model$S))
  mhat <- model$mtgr
  return(sum((y - mhat)^2)*n/(n-traceS)^2)
}

results <- matrix(nrow=length(h.v),ncol=2)
for (i in 1:length(h.v)){
  gcv <- gcvPMSE(seeds$width, seeds$length,h.v[i])
  results[i,] <- c(h.v[i], gcv)
}

best <- which.min(results[,2])
h <- h.v[best]
RK <- 1/(2*sqrt(pi))
hat.fw <- sm.density(width, eval.points=width)$estimate

var.m <- RK * estimators /(n * h.v[best]* hat.fw)

model5 <- locpolreg(width,length, best)
var.hat <- (R.k * s2[1]^2)/(n*best.h*hat.fw)
ordered <- order(seeds$width)
plot(seeds$length~seeds$width)
l1 <- model5$mtgr + var.m
l2 <- model5$mtgr - var.m
lines(seeds$width[ordered], l1[ordered], col = "red")
lines(seeds$width[ordered], l2[ordered], col="green")


str(seeds)

# 3 
mu.g <- rep(mean(groove),length(groove))
model0 <- sm.regression(mu.g,groove)

model1 <- sm.regression(compactness,groove)
R <- function(x){
  if(as.character(x)=='Rosa'){
    return(1)
  }else{
    return(0)
  }
}
R.op <- sapply(seeds$class.factor, R)
x <- R.op+0.001
model2 <- sm.regression(x,groove)

sm.ancova(model1,model2, group=class.factor)
str(seeds)

h1 <- h.select(x = x, y = groove, method = 'cv')


