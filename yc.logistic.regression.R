# Logistic Regresssion

# Label: admit (0-no, 1-yes)
# Predictors: gre, apg and rank

mydata <-  read.csv('data/binary.csv', header=TRUE)
str(mydata)

# Restore categorical variables read-in as integer
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)
str(mydata)

# Since two factor variables, construct a two-way or
# contingency table for examing the data and make sure
# there is no zero value in a data cell.
# A chi2 test (for independence) can be conducted on
# contingency tables to test if a relationship exists
# between categorical variables.
xtabs(~admit+rank, data=mydata)

# Partition Data for training and testing
set.seed(1234)
part  <- sample(2, nrow(mydata), replace=TRUE, prob=c(0.8,0.2))
train <- mydata[part==1,]
test  <- mydata[part==2,]

# Logistic Regression Model
lgr <- glm(admit ~ gre+gpa+rank, family='binomial', data=train)
summary(lgr)

# Drop gre since not significant
lgr <- glm(admit ~ gpa+rank, family='binomial', data=train)
summary(lgr)

#------------
# TRAIN DATA
#------------
# Prediction
p1 <- predict(lgr, train, type='response')
head(p1)

# Use classfication error to evaluate a logistic regression model
pred1 <-  ifelse(p1>0.5,1,0)

cm1 <- table(predicted=pred1, actual=train$admit) # Confusion Matrix
cm1
misClass.train <- 1 - sum(diag(cm1))/sum(cm1) # misclassification
misClass.train
#-----------
# TEST DATA
#-----------
p2 <- predict(lgr, test, type='response')
head(p2)

# Use classfication error to evaluate a logistic regression model
pred2 <-  ifelse(p2>0.5,1,0)

cm2 <- table(predicted=pred2, actual=test$admit) # Confusion Matrix
cm
misClass.test <- 1 - sum(diag(cm2))/sum(cm2) # misclassification
misClass.test

#----------------------
# Goodness-of-fit Test
#----------------------
# Use chi-squared test to determine if they are dependent or not,
# provided, both response and predictors are categorical variables.
chisq.test(test$admit,pred2, correct=FALSE)
