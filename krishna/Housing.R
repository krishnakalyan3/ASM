library('car')
library('sandwich')
library('survival')
library('estimability')
library(sm)
library(doBy)
setwd('/Users/krishna/MIRI/ASM/Analysis')

housing = read.csv('HousingData.csv', sep=';')
housing$total = housing$Sentiment.Favorable + housing$Sentiment.Unfavorable
housing$pos = housing$Sentiment.Favorable/housing$total
housing$neg = housing$Sentiment.Unfavorable/housing$total
str(housing)


summary(housing)

# We see that ProximityClose, ContactFrequent and NormaFavorable are categories
attach(housing)
ProximityClose = as.factor(ProximityClose)
ContactFrequent = as.factor(ContactFrequent)
NormaFavorable = as.factor(NormaFavorable)

m = cbind(Sentiment.Favorable,Sentiment.Unfavorable)
model = glm(m ~ ProximityClose+ContactFrequent+NormaFavorable, family = "binomial")
summary(model)
# With respect to No Proximity , No Contact, No Norm we see that coefficient of CF1 and NF1
# are significant

