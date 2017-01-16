str(seeds)

model1 <- sm.regression(compactness,width)
lines(model1$estimate,width,col='blue')
predict(model1, seeds)
model2 <- loess()

# LOESS
formula <- 'width~compactness'
model2 <- loess(formula)