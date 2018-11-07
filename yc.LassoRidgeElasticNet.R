#------------------------------
# Lasso, Ridge and Elastic Net
#------------------------------

# Collinearity leads to overfitting and solutions include:
# 1. Lasso Regression (L1 Norm) may shrink the coefficients
#         to zeros. Thus helps with feature selection.
# 2. Ridge Regression (L2 Norm) will shrink the coefficients
#         to non-zero values, i.e. all variables are kept.
# 3. Elastic Net Regression - Combine L1 and L2 Norms.

rm(list=ls())

library(psych)        # pairs.panels
library(caret)        # trainControl, resamples
library(glmnet)       # glm
library(mlbench)      # BostonHousing data
data('BostonHousing') # ?BostonHousing

bh <- BostonHousing

str(bh)
# Here the obejctive is to predict medv.
# There is one categorical variable, chas.
head(bh)

#--------------------------------------
# 1. Scatter plot to examine lineraity
#--------------------------------------
pairs.panels(bh[c(-4,-14)], cex=5)  # cex is for font size
# Exclude the categorical and the response variable.
# Here Pearson correlation indicates the extent to
# which two variables are linearly related.

# Visualize the correlation matrix
b <- round(cor(bh[c(-4,-14)]), 2); b
library(corrplot)
corrplot(b, type='upper')


#---------------------------------
# 2. Data and Model Configuration
#---------------------------------
set.seed(123)
pick <- sample(2, nrow(bh), replace=TRUE, prob=c(0.7,0.3))
# Sample 2 groups out of the rows with 70/30 split.

trainSet <- bh[pick==1,]
testSet  <- bh[pick==2,]

#---------------------------------
# 3. Multilinear Regression Model
#---------------------------------

# Training Control Parameters
params <- trainControl(
  method='repeatedcv', # repeated cross-valication
  number=10,           # split data into 9+1 parts
  repeats=5,           # repeat the 10-fold cv 5 times
  verboseIter=TRUE     # verbose trining log
  )

set.seed(321)

# Training
lm <- train(medv~., trainSet, method='lm', trControl = params)

lm; summary(lm); lm$results
plot(lm$finalModel)
# As shown in the summary, there are some variables
# that are not significant. At the same time, this
# model includes many variables possibly with
# collienarity. The next step is to regularize
# the model.

#---------------------
# 3. Ridge Regression
#---------------------
set.seed(12)

# Ridge Regression keeps all the variables,
# while optimizing L2 Norm.
ridge <- train(
  medv~., trainSet, method='glmnet',
  tuneGrid=expand.grid(
    alpha=0,   # Ridge: alpha=0, lasso: alpha=1
    lambda=seq(.0001,1,length=5)),# lambda>>, coeff<<
    trControl=params
  )

plot(ridge); ridge
plot(ridge$finalModel, xvar='lambda', label=TRUE)
plot(ridge$finalModel, xvar='dev',    label=TRUE)
# Verify increasing lambda decreases the cofficients.
# ONce lambda over certain threshold, the cofficients become
# rapidly inflated.
plot(varImp(ridge,scale=FALSE)) # Rank the variables


#---------------------
# 4. Lasso Regression
#---------------------
set.seed(21)
# Lasso Regression may shrink some variables to zeros,
# while optimizng L1 Norm.
lasso <- train(
  medv~., trainSet, method='glmnet',
  tuneGrid=expand.grid(
    alpha=1,   # Ridge: alpha=0, Lasso: alpha=1
    lambda=seq(.0001,0.05,length=5)),# lambda>>, coeff<<
  trControl=params
)

plot(lasso); lasso
# 1. When lambda=seq(.0001,1,length=20), once passed 0.05
# the rmse increases rapdidly. While selected lambda=1e-04
# 2. Reset lambda=seq(.0001,0.05,length=20), while
# selected lamda=0.029 and rmse=4.91
# So, test different lambda sequence to find the optimal.
plot(lasso$finalModel, xvar='lambda', label=TRUE)
plot(lasso$finalModel, xvar='dev',    label=TRUE)
# Verify increasing lambda decreases the cofficients.
# ONce lambda over certain threshold, the cofficients become
# rapidly inflated.
plot(varImp(lasso,scale=FALSE)) # Rank the variables


#--------------------------------
# 5. Elastic Net (EN) Regression
#--------------------------------
set.seed(8)
en <- train(
  medv~., trainSet, method='glmnet',
  tuneGrid=expand.grid(
    alpha=seq(0,1,length=5), # Use a sequence of alpha for EN
    lambda=seq(.0001,1,length=5)),# lambda>>, coeff<<
  trControl=params
)

plot(en); en
# Fomr the plot, it appears the optimal lambda is around 0.1
# which is what EN selected. If not, rerun the model with
# a lambda sequence closer to where the optimal onemay be
# based on interpreting the plot.
plot(en$finalModel, xvar='lambda', label=TRUE)
#  Notice as lambda changes, the # of variables change too.
plot(en$finalModel, xvar='dev',    label=TRUE)
plot(varImp(en,scale=FALSE)) # Rank the variables


#---------------------
# 6. Comparing Models
#---------------------
comparison <- resamples( list(LinearModel=lm, Ridge=ridge, Lasso=lasso, EN=en) )
summary(comparison)
bwplot(comparison)
xyplot(comparison,metric='RMSE')
xyplot(comparison,metric='Rsquared')


#----------------------------------
# 7. Saving a Best and Final Model
#----------------------------------
en$bestTune
# Since alpha=0.1111, so this is more a Ridge
# than a Lasso model.
final <- en$finalModel
coef(final, s=en$bestTune$lambda)

saveRDS(en, 'final.model.rds')


#------------------------------
# 8. Reloading the Saved Model
#------------------------------
# Save the R script file.
# Close the RStudio session and reopen the file.
# Run the library statments, read in and prepare the data.
# Then simply read in the saved rds file to reload the model.

saved.model <- readRDS('final.model.rds')
print(saved.model) # Examine the alpha nad lambda values.


#--------------------------------------
# 9. Making Prediction Using Test Data
#--------------------------------------
ptrain <- predict(saved.model, trainSet)
sqrt(mean( (trainSet$medv - ptrain)^2 ))

ptest <- predict(saved.model, testSet)
sqrt(mean( (testSet$medv - ptest)^2 ))


