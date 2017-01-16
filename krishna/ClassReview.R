setwd('/Users/krishna/MIRI/ASM/Analysis')
battery = read.csv('Bateries.csv', sep =';')
head(battery)

# Dot plots
# Box Plot
# Temp vs Material
# Duration ~ Temp + Material
# No parallel lines 
# Hence Interaction
# Give counter examples
# T = 70 Material 1 -2 inc
# Give opp example
# Conclude interaction

# Response is normally distributed
# Maybe centered about the mean 150
# And we assume normality
# Linar model - Response is normally dist
# Does Duration follow a binomial distribution?

# Tuckeys Method Check on Kaggle
# QQ plot check normailty
# Histogram of residuals show that dont really show normality 
# because of lack of symm
# QQ plot shows that the residuals are grouped

# Fat
## QQ plot to check the normailty of data
## Residual vs Fitted check the homoschedasity. 
# We see non conic distribution

# Housing
# Parameters below are used for model selection
# Deviance small
# Chisquare small
# Doubt on CV
# Residuals should be b/w -2 and 2 
# Larger the log likelhood better the co-efficient
# To compare 2 models that are not nested.
# log likely hood to check 2 models not nested
# model1 = ll1
# model2 = ll2
# max(model1,model2) is the best
# min(AIC (model1 , model)) is the best
# Smaller Residual deviance is good
# If nested compare 2 deviances (subtract residual deviance) or 
# anova(model1,model2, test="Chisq")
# Ho - The model model with the less paramters is better.
# H1 : Model with more parameter is better
# If p values is too big then we do not reject Ho

hist(Duration)
