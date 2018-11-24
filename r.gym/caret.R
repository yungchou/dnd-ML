library(AppliedPredictiveModeling)
library(caret)
# CARET, Classificaiton And Regresson Taining

# dummyVars
# preProcess
# createDataPartition
# makeCluster
# registerDoSNOW
# confusionMatrix

data(solubility)
## The data objects begin with "sol":
ls(pattern = "^solT")
set.seed(2)
sample(names(solTrainX), 8)
trainingData <- solTrainXtrans
## Add the solubility outcome
trainingData$Solubility <- solTrainY

#----------------------------
# Ordinary Linear Regression
#----------------------------
lmFitAllPredictors <- lm(Solubility ~ ., data = trainingData)
summary(lmFitAllPredictors)
lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
head(lmPred1)

lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
defaultSummary(lmValues1)

#--------------------------------
# Robust Linear Regression Model
#--------------------------------
library(MASS)
rlmFitAllPredictors <- rlm(Solubility ~ ., data = trainingData)
ctrl <- trainControl(method = "cv", number = 10)

set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY,
                method = "lm", trControl = ctrl)

lmFit1
xyplot(solTrainY ~ predict(lmFit1),
       ## plot the points (type = 'p') and a background grid ('g')
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

xyplot(resid(lmFit1) ~ predict(lmFit1),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")

corThresh <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]

set.seed(100)
lmFiltered <- train(solTrainXtrans, solTrainY, method = "lm",
                    trControl = ctrl)
lmFiltered

set.seed(100)
rlmPCA <- train(solTrainXtrans, solTrainY,
                method = "rlm",
                preProcess = "pca",
                trControl = ctrl)
rlmPCA

#-----------------------
# Partial Least Squares
#-----------------------
plsFit <- plsr(Solubility ~ ., data = trainingData)
predict(plsFit, solTestXtrans[1:5,], ncomp = 1:2)

set.seed(100)
plsTune <- train(solTrainXtrans, solTrainY,
                 method = "pls",
                 ## The default tuning grid evaluates
                 ## components 1... tuneLength
                 tuneLength = 20,
                 trControl = ctrl,
                 preProc = c("center", "scale"))

#------------------
# Ridge Regression
#------------------
library(elasticnet)
ridgeModel <- enet(x = as.matrix(solTrainXtrans),
                   y = solTrainY,lambda = 0.001)
ridgePred <- predict(ridgeModel,
                     newx = as.matrix(solTestXtrans),
                     s = 1, mode = "fraction",type = "fit")
head(ridgePred$fit)

## Define the candidate set of values
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))
set.seed(100)
ridgeRegFit <- train(solTrainXtrans, solTrainY,
                     method = "ridge",
                     ## Fir the model over many penalty values
                     tuneGrid = ridgeGrid,
                     trControl = ctrl,
                     ## put the predictors on the same scale
                     preProc = c("center", "scale"))
ridgeRegFit

enetModel <- enet(x = as.matrix(solTrainXtrans), y = solTrainY,
                  lambda = 0.01, normalize = TRUE)

enetPred <- predict(enetModel, newx = as.matrix(solTestXtrans),
                    s = .1, mode = "fraction",
                    type = "fit")
## A list is returned with several items:
names(enetPred)
## The 'fit' component has the predicted values:
head(enetPred$fit)

enetCoef<- predict(enetModel, newx = as.matrix(solTestXtrans),
                   s = .1, mode = "fraction",
                   type = "coefficients")

tail(enetCoef$coefficients)
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(solTrainXtrans, solTrainY,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))

