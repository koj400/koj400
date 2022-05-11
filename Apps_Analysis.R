#predict apps on the basis of various statistics
library(ISLR)
data("College")
fix(College)

College=na.omit(College)
dim(College)
#Estimate OLS
lmreg=lm(Apps~., data=College)
summary(lmreg)

#Estimate Ridge Regression
x=model.matrix(Apps~.,College)[,-1]
y=College$Apps
summary(x)

library(glmnet)
ridge.mod=glmnet(x,y, alpha=0, lambda = 405.8404)

# the method standardizes the variables
coef(ridge.mod)

#compare with the OLS regression coefficients
coef(lmreg)

# calculate MSE
set.seed(1)
train=sample(1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0, lambda =405.8404)
ridge.pred=predict (ridge.mod, newx=x[test,])
mean((ridge.pred -y.test)^2)


attach(College)
lm.fit=lm(Apps~., data=College, subset=train)
summary(lm.fit)
mean((Apps-predict(lm.fit, College))[test]^2)

# RMSE of Ridge Regression + LM
rmse_ridge = sqrt(mean((ridge.pred -y.test)^2))
rmse_ridge

rmse_lm = sqrt(mean((Apps-predict(lm.fit, College))[test]^2))
rmse_lm

# using cross validation approach to choose the lambda that leads to lowes MSE
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
bestlam =cv.out$lambda.min
bestlam

# Tree Based Regression
library(tidyverse)
library(tidymodels)
library(dplyr)
library(rsample)

data("College")
attach(College)

college_split=initial_split(College)
college_train=training(college_split)
college_test=testing(college_split)

nrow(college_test)/nrow(College)
nrow(college_train)/nrow(College)

tree_spec <- decision_tree() %>%
  set_engine('rpart') %>%
  set_mode('regression')

train_model <- tree_spec %>%
  fit(formula=Apps~Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+PhD+Expend, data = college_train)
train_model

predictions=predict(train_model, new_data = college_test)
predictions_combined <- predictions %>%
  mutate(true_class = college_test$Apps)
predictions_combined
head(predictions_combined)

# Squared Differences + RMSE of Tree
squared_dif = (predictions_combined$.pred - predictions_combined$true_class)^2
rmse_tree = sqrt(1/nrow(predictions_combined) * sum(squared_dif))
rmse_tree