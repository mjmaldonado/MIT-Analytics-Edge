###########################################
#
# Perform a Framingham Risk Adjustment in R
#
# Author: Michael Maldonado
#
###########################################

# Clear out any other data not useful to this code
rm( list = ls() )

# Get the data
fram = read.csv('framingham.csv')

# Let's take a look at our data
str(fram)

# Let's split out data into a training and test set
library(caTools)
set.seed(1000)
split = sample.split(fram$TenYearCHD, SplitRatio = 0.65)
train = subset(fram, split == TRUE)
test = subset(fram, split == FALSE)

# Now let's build a logistic regression model
fram_log = glm( TenYearCHD ~ ., data = train, family = binomial() ) # (.) - Use all of the variables
summary(fram_log)

# Now let's use this model to make predictions on our test set
predictTest = predict( fram_log, type = "response", newdata = test ) # "response" gives us probabilities

# Let's use a threshold value of 0.5 to create a confusion matrix
conf_matrix = table(test$TenYearCHD, predictTest > 0.5)

# What is the accuracy of our model?
fram_log_accuracy = (conf_matrix[1,1] + conf_matrix[2,2]) / sum( conf_matrix[] )
fram_log_accuracy

# How does this compare to our baseline accuracy?  The most frequent prediction is 0, so our model would predict no CHD the majority of the time anyways.
fram_log_baseline = sum(conf_matrix[1,]) / sum(conf_matrix[])
fram_log_baseline

# So we can see, our model is barely better than the baseline accuracy.  However, do we have a valuable model out-of-sample if we vary the threshold?
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# This means that the model can differentiate between low risk patients and high risk patients pretty well.

# Quick question - What is the sensitivity of our logistic regression model on the test set, using a threshold of 0.5
# Recall, sensitivity = TP / (TP + FN)
conf_matrix[2,2] / sum(conf_matrix[2,])

# What is the specificity using the same threshold?
# Recall, specificity = TN / (TN + FP)
conf_matrix[1,1] / sum(conf_matrix[1,])

# Keep in mind, this is only using internal validation.  It's important to perform external validation, as well.
# What does this mean?  Well, if I want to predict obesity risk, and I have a dataset that only contains individuals who live in Northern California,
# this may not necessarily generalize well to a prediction for the entire United States.