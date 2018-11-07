#-------------------
# Multicollinearity
#-------------------

# Multicillinearity tends to increase the variance of
# regresion coefficients and leads to unstable estimates.

# Use VIF, Variance Inflation Factor, to assess the presence
# of multilcollineraity, denoted by VIF > 4 or VIF > 10 as
# rule of thumb. In such case, keeping only one of the two
# highly correlated variables is a common practice.
# Some do question the validity of such a practice.
#
# https://link.springer.com/article/10.1007/s11135-006-9018-6
# A Caution Regarding Rules of Thumb for Variance Inflation Factors

library(faraway)
data(divusa) # ?divusa
str(divusa)
head(divusa)

mydata <-  data.frame(divusa[,-1])  # drop year column

#---------------------------------------------
# 1. Scatter plot for an overview of the data
#---------------------------------------------
pairs.panels(mydata, cex=2)  # cex is for fonts

# Examine the correlation matrix
m <- round(cor(mydata), 2); m  # round it to 2 decimal points
# A high correlation vlaue indicates the associated two variables
# are correlated, e.g. 1.00, 0.67, -0.72, etc.

# Visualize the correlation matrix
library(corrplot)
corrplot(m, type='upper')

#-----------------------------------------------
# 2. Considering a multilinera regression model
#-----------------------------------------------
full <- lm(divorce~., mydata); full
summary(full)
# unemployed and military are not signaficant.
# Adjusted R^2 = 0.9152
# F-stats' p<<0.05

#-------------------------------------------
# 3. Examine multicollinearity of the model
#-------------------------------------------
vif(full)
# All variables are with VIFs < 5 and therefore
# assuming multicollinearity is here not an issue.
