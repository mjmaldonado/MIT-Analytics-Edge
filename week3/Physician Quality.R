################################################################
#
# Attempt to predict physician quality
#
# Author: Michael Maldonado
#
# Working through the lecture material for logisitic regression
#
###############################################################

quality = read.csv('quality.csv')

#################################################################################################
#
# Column guide
#
# MemberID              numbers the patients from 1 to 131, and is just an identifying number.
# InpatientDays         is the number of inpatient visits, or number of days the person spent in the hospital.
# ERVisits              is the number of times the patient visited the emergency room.
# OfficeVisits          is the number of times the patient visited any doctor's office.
# Narcotics             is the number of prescriptions the patient had for narcotics.
# DaysSinceLastERVisit  is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 
# Pain                  is the number of visits for which the patient complained about pain.
# TotalVisits           is the total number of times the patient visited any healthcare provider.
# ProviderCount         is the number of providers that served the patient.
# MedicalClaims         is the number of days on which the patient had a medical claim.
# ClaimLines            is the total number of medical claims.
# StartedOnCombination  is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).
# AcuteDrugGapSmall     is the fraction of acute drugs that were refilled quickly after the prescription ran out.
# PoorCare              is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.
#
#################################################################################################

# In this video we are using sample.split() to partition our data into test and training.
# Another method is as follows:
#
# split = sample( 1:nrow(data), size = 0.7 * nrow(data) )
# train = data[split,]
# test  = data[-split,]

str(quality)

# What does are current data look like for quality of care delivered?
table(quality$PoorCare)

# We will assume that all patients receive high quality care, becasue this is the mode of our dependent variable.
# This means that we are going to try and predict quality of care > (98/131) times, or ~75%

# There many ways to split data into training and test sets.  For this particular problem we will use sample.split() from caTools
install.packages('caTools')

set.seed(88) # For reproducibility.  Would not want to do this for a production model
library('caTools')
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

quality_train = subset(quality, split == TRUE)
quality_test  = subset(quality, split == FALSE)

# Let's build our logistic regression model
quality_log = glm(PoorCare ~ OfficeVisits + Narcotics, data = quality_train, family = binomial)
summary(quality_log)

# Make a prediction on the training data
predict_train = predict( quality_log, type = "response" ) #this tells predict to give us predicted probabilities
summary(predict_train)

# Let's see if we're predicting higher probabilities for the actual poor care cases as we expect.
tapply(predict_train, quality_train$PoorCare, mean)

# We can see that for all of the true poor cases we are predicting an average probability of about 0.44.
# We can also see that for the true good cases we are predicting an average probability of about 0.19.
# This is a good sign because it looks like we're predicting
# a higher probability for the actual poor care cases.

# Quick question
quality_log_qq = glm( PoorCare ~ StartedOnCombination + ProviderCount, data = quality_train, family = binomial )
summary(quality_log_qq)

# How do we measure the quality of our logistic regression model?
# We can use a confusion matrix!
confusion_matrx = table(quality_train$PoorCare, predict_train > 0.5)

# Let's compute the sensitivity and the specificity
# Sensitivity - the True Positive Rate = True Positives / (True Positives + False Negatives)
# Specificity - the True Negative Rate = True Negatives / (True Negatives + False Positives)

Sensitivity = confusion_matrx[2,2] / ( sum(confusion_matrx[2,]) )
Specificity = confusion_matrx[1,1] / ( sum(confusion_matrx[1,]) )

# Now let's see what happens when we change the threshold to 0.7
confusion_matrx_70 = table(quality_train$PoorCare, predict_train > 0.7)

Sensitivity_70 = confusion_matrx_70[2,2] / ( sum(confusion_matrx_70[2,]) )
Specificity_70 = confusion_matrx_70[1,1] / ( sum(confusion_matrx_70[1,]) )

# As you can see, by increasing the threshold, our sensitivity went down (our true positive rate)
# and our specificity went up (our true negative rate)

# Let's see what happens if we decrease the threshold
confusion_matrx_20 = table(quality_train$PoorCare, predict_train > 0.2)

Sensitivity_20 = confusion_matrx_20[2,2] / ( sum(confusion_matrx_20[2,]) )
Specificity_20 = confusion_matrx_20[1,1] / ( sum(confusion_matrx_20[1,]) )

# So with a lower threshold, our sensitivity went up and our specificity went down.
# Well great!  But, how do I know which threshold to pick?

# Use a ROC curve!
# Select the best threshold for the trade-off you want to make.
install.packages('ROCR')
library('ROCR')

ROCRpred = prediction( predict_train, quality_train$PoorCare)
ROCRperf = performance( ROCRpred, "tpr", "fpr" )
plot(ROCRperf, colorize = TRUE, main = "Receiver Operator Characteristic Curve", print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

# Let's summary, again, the different ways we can measure model performance for logistic regressions and similar models

# AUC = Area Under the Curve (the ROC Curve)
#       1.0 = Perfect
#       0.5 = Random guessing (not good)

# Confusion Matrix
#
# N  = Number of observations
# TP = True Positives
# TN = True Negatives
# FP = False Positives
# FN = False Negatives
#
# Overall Accuracy          = (TN + TP) / N
# Overall Error Rate        = (FP + FN) / N
# Sensitivity               = TP / (TP + FN)
# Specificity               = TN / (TN + FP)
# False Negative Error Rate = FN / (TP + FN)
# False Positive Error Rate = FP / (TN + FP)

# Quick Question - Compute the test set predictions
# What is the AUC of this model on the test set?
# First we need to run a prediction with one of our models
predictTest = predict(quality_log, type = "response", newdata = quality_test)

# Now generate the ROC
ROCPredTest = prediction( predictTest, quality_test$PoorCare )

# Now we can compute the AUC
auc = as.numeric(performance(ROCPredTest, "auc")@y.values)
auc

# Looks like our model is performing fairly well!