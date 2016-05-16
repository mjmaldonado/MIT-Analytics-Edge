#####################################
#
# Classification and Regression Trees
#
# Supreme Court Forecasting Project
#
# Author: Michael Maldonado
#
#####################################

# Load the data
stevens = read.csv('stevens.csv')

# Take a look at the structure of the data
str(stevens)

# Let's split the data into a training and test set
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)

# Now let's build the CART model
# First let's get some useful packages
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# Now we can build the model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25) #Build a classification tree

# Let's plot the tree
prp(StevensTree)

# Now let's see how our model performs on the test set
PredictCART = predict(StevensTree, newdata = test, type = "class")

# Now let's determine the accuracy of the model, by first building a confusion matrix
conf_mat = table(test$Reverse, PredictCART)
(conf_mat[1,1] + conf_mat[2,2]) / sum(conf_mat)

# To evaluate our model, let's take a look at the ROC curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = test)
pred = prediction(PredictROC[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Quick question - what is the AUC?
auc = as.numeric(performance(pred, "auc")@y.values)
auc

# What happens to the number of splits if we decrease the bucket size to 5?
StevensTree_5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 5) #Build a classification tree
prp(StevensTree_5)

# What happens to the number of splits if we increase the bucket size to 100?
StevensTree_100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100) #Build a classification tree
prp(StevensTree_100)

#########################################
# Part 2 - Random Forests
#########################################

# install an useful package
install.packages('randomForest')
library(randomForest)

# Let's now train our random forest
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)

# uh oh, we got a warning message.  This is because we need to make sure our dependent variable is a factor.  Otherwise, it won't run the regressions.
# This is becasue the randomForest class does not have an argument method, so when we want to do a classification problem, this is why our outcome needs to be a factor.

train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

# Now re-run line 73 (StevensForest)
# Worked like a charm!

# Let's now see how well our model is doing on the test set
PredictForest = predict(StevensForest, newdata = test)
conf_mat_forest = table(test$Reverse, PredictForest)
forest_accuracy = (conf_mat_forest[1,1] + conf_mat_forest[2,2]) / sum(conf_mat_forest)
forest_accuracy

# So we gained a bit of accuracy on the test set using a randomForest, great news!

# Quick question - let's see what happens when we set the seed to different values an re-train our randomForest models
set.seed(100)
StevensForest_seed_100 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)

# What is the accuracy of this model?
PredictForest_100 = predict(StevensForest_seed_100, newdata = test)
conf_mat_forest_100 = table(test$Reverse, PredictForest_100)
forest_accuracy_100 = (conf_mat_forest_100[1,1] + conf_mat_forest_100[2,2]) / sum(conf_mat_forest)
forest_accuracy_100

# Now how about with a seed of 200?
set.seed(200)
StevensForest_seed_200 = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)

# What is the accuracy of this model?
PredictForest_200 = predict(StevensForest_seed_200, newdata = test)
conf_mat_forest_200 = table(test$Reverse, PredictForest_200)
forest_accuracy_200 = (conf_mat_forest_200[1,1] + conf_mat_forest_200[2,2]) / sum(conf_mat_forest)
forest_accuracy_200

# As we see here, the random component of the random forest method can change the accuracy.
# The accuracy for a more stable dataset will not change very much, but a noisy dataset can be significantly affected by the random samples.

# K-fold Cross-Validation
install.packages('caret')
install.packages('e1071')
library('caret')
library('e1071')

# Let's define how many folds we want
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01,  0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# You'll get a table output of the different cross-validated accuracies for different cp parameters
# the first column gives the cp parameter that was test
# the second columngives the cross-validation accuracy for that cp value
# the accuracy starts lower and then increases, and then decreases again
# at the bottom of the output, it will tell you which cp was used to select the model

# This is the cp value we want to use in our CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.18)

# Now let's make predictions on our test set
predictCV = predict(StevensTreeCV, newdata = test, type = "class")
conf_mat_cv = table(test$Reverse, predictCV)
cv_acc = (conf_mat_cv[1,1] + conf_mat_cv[2,2]) / sum(conf_mat_cv)
cv_acc

# So the accuracy has improved once again!  So it looks like cross-validation was quite useful to boost the accuracy of our model

# Quick question - plot the tree we created using cross-validation.  How many splits does it have?
prp(StevensTreeCV)

# The tree with the best accuracy only has one split! When we were picking different minbucket parameters before, it seemed like this tree was
# probably not doing a good job of fitting the data. However, this tree with one split gives us the best out-of-sample accuracy.
# This reminds us that sometimes the simplest models are the best!


######################################################
#
# Let's now make some prediction on health care costs
#
######################################################

# clear some space
rm( list = ls() )

# Load in the data
claims = read.csv('ClaimsData.csv')

# take a look at the structure of our data
str(claims)

# Let's use 2008 data to try and predict costs in 2009

# Let's look at the percentage of patients in each cost buckets
table(claims$bucket2009) / nrow(claims)

# Let's divide the data in to a test set and training set
library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009, SplitRatio = 0.6)
train = subset(claims, spl == TRUE)
test = subset(claims, spl == FALSE)

# Quick question - what is the average age of patients in the training set?
mean(train$age)

# What proportion of people in the training set had at least one diagnosis code for diabetes?
mean(train$diabetes)

# Let's not take a look at our baseline method.  The base line method would predict that the cost bucket would be the same in 2008 and 2009
conf_mat_bl = table(test$bucket2009, test$bucket2008)

# The accuracy is the sum of the diagonal divided by the total obersvations in the dataset
accuracy = (conf_mat_bl[1,1] + conf_mat_bl[2,2] + conf_mat_bl[3,3] + conf_mat_bl[4,4] + conf_mat_bl[5,5]) / sum(conf_mat_bl) 

# How about the penalty error?  First we need to create a penalty matrix
penalty_matrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
penalty_matrix

penalty_error = sum(as.matrix(conf_mat_bl) * penalty_matrix) / nrow(test)

# So our goal will be to create a model with an accuracy >68% and a penalty error <74%

# Quick question - Instead of our selected baseline method, what if our baseline was to predict the most frequent outcome for all observations.
# What is the accuracy of this model?
conf_mat_bl2 = table(test$bucket2009)
accuracy_bl2 = conf_mat_bl2[1] / sum(conf_mat_bl2)

# What is the penalty error of this model?
penalty_error_bl2 = sum(as.matrix(conf_mat_bl2) * penalty_matrix[,1]) / nrow(test)

# Now that we have our baseline, let's make some predictions
library(rpart)
library(rpart.plot)

claimsTree = rpart(bucket2009 ~
                     age +
                     arthritis +
                     alzheimers +
                     cancer +
                     copd +
                     depression +
                     diabetes +
                     heart.failure +
                     ihd + kidney +
                     osteoporosis +
                     stroke +
                     bucket2008 +
                     reimbursement2008,
                   data = train,
                   method = "class",
                   cp = 0.00005
                   )
# This will take a view seconds...

# Now that our models done, let's take a look at our tree
prp(claimsTree)

# We have a huge tree here.  The large number of observations in the data set.
# We also have a 5-class classifcation problem, vs a binary classification problem
# While this hurts the interpretability of the model, we are still abe to explain each
# part of the tree, according to the splits

# Now let's make predictions on the test set
predictTest = predict(claimsTree, newdata = test, type = "class")

# How well did we do?
conf_mat_tree = table(test$bucket2009, predictTest)

tree_accuracy = (conf_mat_tree[1,1] + conf_mat_tree[2,2] + conf_mat_tree[3,3] + conf_mat_tree[4,4] + conf_mat_tree[5,5]) / nrow(test)

# What is our pentaly error?
tree_penalty_error = sum(as.matrix(conf_mat_tree) * penalty_matrix) / nrow(test)

# We can see that our accuracy has increased, but our penatly error has increased, also.
# Why did our pentaly error go up?  By default, rpart will try to maximize the model accuracy.  For this reason, we don't expect the model to
# improve over the penalty error (by default).
# How can we improve upon this?

# We can tell rpart to use our penalty_matrix as the loss parameter
# Now that the model knows how we are penalizing the classification, it might select different
# tree splits to minimize the highest error classifications

claimsTree = rpart(bucket2009 ~
                     age +
                     arthritis +
                     alzheimers +
                     cancer +
                     copd +
                     depression +
                     diabetes +
                     heart.failure +
                     ihd + kidney +
                     osteoporosis +
                     stroke +
                     bucket2008 +
                     reimbursement2008,
                   data = train,
                   method = "class",
                   cp = 0.00005,
                   parms = list(loss = penalty_matrix)
)

# Now let's make predictions on the test set, again
predictTest = predict(claimsTree, newdata = test, type = "class")

# How well did we do?
conf_mat_tree = table(test$bucket2009, predictTest)

tree_accuracy = (conf_mat_tree[1,1] + conf_mat_tree[2,2] + conf_mat_tree[3,3] + conf_mat_tree[4,4] + conf_mat_tree[5,5]) / nrow(test)

# What is our pentaly error?
tree_penalty_error = sum(as.matrix(conf_mat_tree) * penalty_matrix) / nrow(test)

# Now our accuracy has decreased, but so has our penalty error (by quite a bit)
# We will consider this a better model than our baseline due to the significant decrease in penalty error, and the small decrease in accuracy

# Quick question - did the second CART model predict more or less obeservations as proportion of the total?  Why?
# Our first CART model predict 78.6% of the observations to be in bucket 1

table(predictTest)[1] / nrow(test) # CART 2 predicts 58.1% of the obersvations to be in bucket 1

# This is because, according to our pentaly matrix, some of the worst types of errors are to predict
# bucket 1 when the actual cost bucket is higher.  Therefore, the model with the penalty matrix predicted bucket 1 less frequently.
