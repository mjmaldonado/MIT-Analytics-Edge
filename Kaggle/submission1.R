################################################
#
# Kaggle competition - predicting whether a person will vote democrat or republican
#
# Author: Michael Maldonado
#
# The data is provided by MIT through the competition website, train2016 and test2016.
#
# The goal is to maximize Categorization Accuracy (the number of category predictions that match the actual category,
# divided by the number of observations)
#
################################################

# read in the training and test set data
train = read.csv('train2016.csv')
test  = read.csv('test2016.csv')

# let's perform some variable transformation
library(dplyr)

train = tbl_df(train)
test = tbl_df(test)

# take a look at the structure of our data
str(train)*
str(test)

# now let's get some summary stats on our data
summary(train)
summary(test)

table(train$YOB) # it appears there are some outlier (mistake values) in the training set YOB, let's remove these
train = filter(train, between(YOB, 1900, 2003) | is.na(YOB))
test = filter(test, between(YOB, 1900, 2003) | is.na(YOB))

# let's impute the rest of the missing values for YOB
# first I'm going to combine the test and training set, then impute, then split them back apart
train$dataset = 'train'
test$dataset = 'test'
test$Party = NA

# now create the full dataset
full = rbind(train, test)
full = tbl_df(full)

# grab the mice package for imputation
library(mice)
set.seed(4)

# perform the imputation
full = full %>% mice %>% complete()

# now break the data back into the training and test set
train = full %>% filter(dataset == 'train')
test = full %>% filter(dataset == 'test')

# let's see what the imputation came up with for Party - I plan to use this as a submission just to see what happens
table(test$Party)

# remove the full dataset
rm(full)

# remove the columns created to make the full dataset
train$dataset = NULL

# store submission1
submission1 = data.frame(USER_ID = test$USER_ID, Predictions = test$Party)

# write the first submission to a csv
write.csv(submission1, "Submission1.csv", row.names=FALSE)

# now clean up the test set for building models
test$dataset = NULL; test$Party = NULL

# now let's create a variable for age, then age group
cy = Sys.Date() %>% format("%Y") %>% as.numeric()

# calculate the age and add it to the data table
train = train %>% mutate(Age = cy - YOB) %>% select(USER_ID, YOB, Age, 3:ncol(train))
test = test %>% mutate(Age = cy - YOB) %>% select(USER_ID, YOB, Age, 3:ncol(test)) # no Party variable in the test set

summary(train$Age); summary(test$Age)

# now let's create an age group variable
train = train %>%
  mutate(AgeGroup = cut(Age, breaks = c(-Inf,seq(20,65,by = 5),Inf), labels = c('<=20', '21-35', '26-30', '31-35', '36-40', '41-45', '46-50', '51-55', '56-60', '61-65', '>=65'), ordered_result = TRUE)) %>%
  select(USER_ID, Age, AgeGroup, 4:(ncol(train)))

test = test %>%
  mutate(AgeGroup = cut(Age, breaks = c(-Inf,seq(20,65,by = 5),Inf), labels = c('<=20', '21-35', '26-30', '31-35', '36-40', '41-45', '46-50', '51-55', '56-60', '61-65', '>=65'), ordered_result = TRUE)) %>%
  select(USER_ID, Age, AgeGroup, 4:(ncol(test)))

summary(train$AgeGroup)
summary(test$AgeGroup)
# success!

# define a baseline accuracy (user votes democrat)
accuracy.bl = max(table(train$Party)) / nrow(train)

# Now let's perform feature selection.
for (name in names(select(train,-USER_ID, -Party))) {
  # create a table for each column in the dataset
    x = table(train$Party, train[[name]])
    
  # check whether the unanswered questions help differentiate away from democrat (the baseline) for any variable
    y = ifelse( ( x[1,1] / x[2,1] < accuracy.bl) ,print(x),0)
    
  # tell me which variables those are
    ifelse(y == 0, "Unanswered", print(name))
}

# this tells me that there are no variables outside of the age variable where the value = '' that differentiate better than the baseline

# now let's take a look at the variables in comparison to the Party variable to see what interesting information can be gleaned
for (name in names(train)) {
  print( name )
  print( table ( train$Party, train[[name]] ) )
}

# After reviewing the above code, the following variable appear they may be useful for prediction:
  # AgeGroup
  # Gender
  # Income
  # HouseholdStatus
  # EducationLevel
  # Q122771 - Did you attend public or private school?
  # Q120379 - Do you have (or paln to pursue) a Masters or PhD?
  # Q120472 - Science or Art?
  # Q118232 - Idealist or Pragmatist?
  # Q116881 - Happy or Right?
  # Q115611 - Do you personally own a gun?
  # Q115899 - Circumstances beyond own control or result of own decisions
  # Q115195 - Live within 20 miles of metropolitan area?
  # Q113181 - Meditate or Pray regularly?
  # Q109244 - Are you a feminist?
  # Q108617 - Single-parent household?
  # Q106272 - Own any power-tools?
  # Q106388 - Do you work 50+ hours/week?
  # Q101163 - Which parent "wore the pants" in your household?
  # Q99480  - Did you parents spank you as a form of discipline/punishment?
  # Q98869  - Does life have a purpose?
  # Q98197  - Pray or Meditate regularly?

# Now let's reduce the training and test set to the selected variables
train.model = train %>%
  select(
    USER_ID,
    Party,
    AgeGroup, 
    Gender, 
    Income, 
    HouseholdStatus, 
    EducationLevel,
    Q98197,
    Q98869,
    Q99480,
    Q101163,
    Q106272,
    Q108617,
    Q109244,
    Q113181,
    Q115611,
    Q116881,
    Q118232,
    Q120379,
    Q120472,
    Q122771
)

test.model = test %>%
  select(
    USER_ID,
    AgeGroup, 
    Gender, 
    Income, 
    HouseholdStatus, 
    EducationLevel,
    Q98197,
    Q98869,
    Q99480,
    Q101163,
    Q106272,
    Q108617,
    Q109244,
    Q113181,
    Q115611,
    Q116881,
    Q118232,
    Q120379,
    Q120472,
    Q122771
)

library(reshape2)
train.model = train.model %>% select(-Party) %>% recast(USER_ID ~ variable + value, id.var = 1, fun.aggregate = function(x) (length(x) > 0) + 0L)
test.model  = recast(test.model, USER_ID ~ variable + value, id.var = 1, fun.aggregate = function(x) (length(x) > 0) + 0L)

# now I need to get the Party variable back in the training model
# first I'll create a reference table
party.join = train %>% select(USER_ID, Party)

# now join back to the training model data
train.model = inner_join(train.model, party.join, by = "USER_ID")

# and now convert Party into a factor
train.model$Party = as.factor(train.model$Party)

str(train.model)
str(test.model)

# set up our model matrix and dependent variable
# I don't want USER_ID in my predictions as it won't be meaningful in any way
train.model = select(train.model, -USER_ID)
test.model  = select(test.model, -USER_ID)

x = model.matrix(Party ~ . -1, data = train.model)
y = train.model$Party

# now it's time to build some models
# Logistic Regression
model.LR = glm(Party ~ ., family = "binomial", data = train.model)
summary(model.LR)
pred.LR = predict(model.LR, type = "response")
conf.mat.LR = table(train.model$Party, pred.LR > 0.5)
accuracy.LR = sum( diag( conf.mat.LR ) ) / nrow( train.model )

# CART
library(rpart)
model.CART = rpart(Party ~ ., method = "class", data = train.model)
pred.CART = predict(model.CART, type = "class")
conf.mat.CART = table(train.model$Party, pred.CART)
accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow( train.model )

# Random Forest
library(randomForest)
set.seed(4)
model.RF = randomForest(x,y, method = "class", data = train.model)
pred.RF = predict(model.RF, type = "class")
conf.mat.RF = table(train.model$Party, pred.RF)
accuracy.RF = sum( diag( conf.mat.RF ) ) / nrow( train.model )

# some of these models can be computationally expensive, so I'm going to try and leverage some parallel processing via the 'doParallel' package
library(doParallel)
workers = makeCluster(detectCores(), type = 'PSOCK') # my computer has 4 cores
registerDoParallel(workers)

library(caret)
gbmGrid = expand.grid(interaction.depth = seq(1,7, by = 2),
                      n.trees = seq(100,1000, by = 50),
                      shrinkage = c(0.01, 0.1),
                      n.minobsinnode = 10
                      )

ctrl = trainControl(method = "boot", # bootsrap
                    number = 25, # repeat 25 times
                    # classProbs = TRUE, # estimate class probabilities
                    # summaryFunction = twoClassSummary,
                    index = createResample(train.model$Party, 25)
                    )

set.seed(4)
gbm.train = train(x,y,
                  method = "gbm",
                  trControl = ctrl,
                  verbose = FALSE,
                  # the default metric optimized is accuracy
                  tuneGrid = gbmGrid
                  )
# the final model chose 950 trees, with an interaction.depth = 7, and n.minnobinnode = 10
gbm.train

pred.gbm = predict(gbm.train, newdata = test.model)

submission2 = data.frame(USER_ID = test$USER_ID, Predictions = pred.gbm)

write.csv(submission2, "Submission2.csv", row.names=FALSE)

# Now let's try deepboost
db.train = train(x,y,
                 method = "deepboost",
                 trControl = ctrl,
                 verbose = FALSE
                 )
db.train

# let's retry randomForest with 10-fold cross-validation
set.seed(4)
rf.train = train(x,y,
                 method = "rf",
                 trControl = ctrl,
                 verbose = FALSE
                 )
rf.train

# Extreme Gradient Boost
set.seed(4)
xgb.train = train(x,y,
                  method = "xgbTree",
                  trControl = ctrl,
                  verbose = FALSE
                  )
xgb.train

# support vector machine
svm.train = train(x,y,
                  method = "svmRadial",
                  trControl = ctrl,
                  verbose = FALSE
                  )

svm.train

# Modeled Average Neural Network
nnet.train = train(x,y,
                   method = "avNNet",
                   trControl = ctrl,
                   verbose = FALSE
                   )

nnet.train

# now let's try an ensemble of the previous models
library(caretEnsemble)

ctrl = trainControl(method = "boot", # bootsrap
                    number = 50, # repeat 25 times
                    # classProbs = TRUE, # estimate class probabilities
                    # summaryFunction = twoClassSummary,
                    savePredictions = TRUE, # this needs to be set to true in order for the caretEnsemble to work.
                    index = createResample(train.model$Party, 25)
)

model.list = caretList(x,y,
                        methodList = c("gbm", "deepboost", "rf", "xgbTree", "svmRadial", "avNNet"),
                        # metric = "ROC", # classProbs = TRUE must be set for this, however, deepboost does not produce class probabilities
                        trControl = ctrl,
                        verbose = FALSE
                        )

# let's take a look at how these models correlate with one another.  This will be important in determining whether or not it makes sense to include
# all of the models together (i.e. highly correlated models will not make sense to ensemble together)
modelCor(resamples(model.list))

model.ensemble = caretStack(model.list,
                              # According to the caretEnsemble Vignette, a non-linear ensemble seems to work when you have:
                                # Lots of data(which in this case we do not)
                                # Lots of models with similar accuracies
                                # Un-correlated models, where each model seems to capture a different aspect of the data and different models perform best on different subsets of the data
                              method = "glm",
                              # metric = "ROC",
                              trControl = trainControl(
                               method = "cv",
                               number = 10
                               # summaryFunction = twoClassSummary
                               )
                            )
summary(model.ensemble)


# turn parallel processing off
stopCluster(workers)
