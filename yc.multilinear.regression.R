#------------------------------
# Multilinear Regression Model
#------------------------------

rm(list=ls())

vehicle <- read.csv("data/vehicle.csv")
str(vehicle)
head(vehicle)

#--------------------------------------
# 1. Scatter plot to examine lineraity
#--------------------------------------
library(psych)
pairs.panels(vehicle[3:5], cex=2)  # cex is for fonts
# There appears apparnet linearity between lc and lh.

# Visualize the correlation matrix
v <- round(cor(vehicle[3:5]), 2); v
#library(corrplot)
#corrplot(v, type='upper')

# 0.98 indicates lc and lh have a highly positive correlation.

#---------------------------------------------
# 2. Considering: y = b0 + b1*lh + b2*Mileage
#---------------------------------------------
full <- lm(lc~Mileage+lh, vehicle); summary(full)
cat('b0 = ', full$coefficients['(Intercept)'],
    '\nb1 = ',full$coefficients['Mileage'],
    '\nb2 = ',full$coefficients['lh'])

# mileage (p=0.201) is not signaficant.
# R^2 = 0.95
# F-stats' p<<0.05

#------------------
# 3. Reduced Model
#------------------
reduced <- lm(lc~lh, vehicle); summary(reduced)

cat('b0 = ', reduced$coefficients['(Intercept)'],
    '\nb1 = ',reduced$coefficients['lh'])
# The model: lc = -0.2359 + 73.5088*lh


#---------------------------------------------------
# 4. Comparing Models (ANOVA: Analysis of Variance)
#---------------------------------------------------
anova(reduced, full)
# The p-value is not significant, i.e. the reduced one is significant.

#-------------------------------------------------------
# 5. Make prediction with the rduced model (R^2 = 0.95)
#-------------------------------------------------------
predict(reduced, data.frame(lh=10), interval='confidence')
# predicting the labor cost for 10 labor hours with confidence interval
# fit or the average = 734, and range form 728 to 740