#-------------------------------
# Multi-Linear Regression Model
#-------------------------------
vehicle <- read.csv("N:/dnd-ML/data/vehicle.csv")
str(vehicle)
head(vehicle)

#--------------------------------------
# 1. Scatter plot to examine lineraity
#--------------------------------------
library(psych)
pairs.panels(vehicle[3:5], cex=2)  # cex is for fonts
# Assume we ony interested in lc, lh and mileage

#---------------------------------------------
# 2. Considering: y = b0 + b1*lh + b2*Mileage
#---------------------------------------------
full <- lm(lc~Mileage+lh, vehicle)
cat('b0 = ', full$coefficients['(Intercept)'],
    '\nb1 = ',full$coefficients['Mileage'],
    '\nb2 = ',full$coefficients['lh'])
summary(full)
# mileage (p=0.201) is not signaficant.
# R^2 = 0.95
# F-stats' p<<0.05

#------------------
# 3. Reduced Model
#------------------
reduced <- lm(lc~lh, vehicle)
cat('b0 = ', reduced$coefficients['(Intercept)'],
    '\nb1 = ',reduced$coefficients['lh'])
# The model: lc = -0.2359 + 73.5088*lh
summary(reduced)

#-------------------------------------------------
# 4. Compare Models (ANOVA: Analysis of Variance)
#-------------------------------------------------
anova(reduced, full)
# The p-value is not significant, i.e. the reduced one is significant.

#-------------------------------------------------------
# 5. Make prediction with the rduced model (R^2 = 0.95)
#-------------------------------------------------------
predict(reduced, data.frame(lh=10), interval='confidence')
# predicting the labor cost for 10 labor hours with confidence interval
# fit or the average = 734, and range form 728 to 740