rm(list=ls())
library(gplots)

setwd('/Users/krishna/MIRI/ASM/Analysis')
fat_data = read.csv('Fat.csv', sep = ';')

attach(fat_data)
# Do I need to balance data set
# Do I need to standarise the dataset?, Normalize
head(fat_data)

# Fat Column convert from int to factor 
Fat = as.factor(Fat)

# Lets boxplot data
plot(Fat,Absorbedgr)
plotmeans(Absorbedgr~Fat)

 # Observations
# a) Fat 2 has a highest average absorption rate
# b) Fat 4 has the lowest average absortion rate


model.lm <- lm(Absorbedgr ~ Fat )
summary(model.lm)
plot(Absorbedgr)
plot(model.lm)
# Observations
# According to the model Fat1, Fat2 is significant
# Residulas seem to be centered around the mean
# Q-Q plots confirm normal
# The residual vs fitted plots show that the values are unccorelated
# This dosent seem to be a very good model as the 
# Ajusted R square is only 0.365 

(anova(model.lm))

# Coeffienct Variation - What to write about this
# Balance
# Forward selection