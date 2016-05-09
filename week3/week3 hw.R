######################################
#
# Week 3 Homework
#
# Author: Michael Maldonado
# 
# Create date: 2016-05
#
######################################

############################################
# Problem Set 1 - Predicting song popularity
############################################

#####################################
#
# Column guide
#
# year              = the year the song was released
# songtitle         = the title of the song
# artistname        = the name of the artist of the song
# songID            = song identifier
# artistID          = artist identifier
# timsignature      = a variable estimating the time signature of the song
# timesignature_confidence  = the confidence in the estimate
# loudness          = a continuous variable indicating the average amplitude of the audio in decibels
# tempo             = the estimated beats per minute of the song
# tempo_confidence  = the confidence in the estimate
# key               = a variable with twelve levels indicating the estimated key of the song
# key_confidence    = the confidence in the estimate
# energy            = a variable that represents the overall acoustic energy of the song, using a mix of features such as loudenss
# pitch             = a continuous variable that indicates the pitch of the song
# timbre_x_min/max  = variables that indicate the min/max values over all segments for each of the twelve values in the timbre vector (resulting in 24 continous variables)
# Top10             = a binary variable indicating whether or not the song made it to the Top 10 of the Billboard Hot 100 Chart (1 = yes, 0 = no)
#
######################################

# clear space
rm( list = ls() )

# read in the data
song = read.csv('songs.csv')

# 1.1.1 - How many observations are from the year 2010
nrow(subset(data, year == 2010))

# 1.1.2 - How many songs are accounted for by Michael Jackson
mj = subset(song, artistname == "Michael Jackson")
nrow(mj)

# 1.1.3 - Which of these songs by Michael Jackson made it to the Top 10?
subset(mj, Top10 == 1)$songtitle

# 1.1.4 - What are the values that occur in timesignature?
sort(unique(song$timesignature))

# Which timesignature value is the most frequent?
sort(table(song$timesignature))

# 1.1.5 - Of all songs in the dataset, which has the highest tempo?
song$songtitle[which.max(song$tempo)]

# 1.2.1 - Split the data into a test and training set, where the training set is songs for years 2009 and prior
train = subset(song, year <= 2009)
test = subset(song, year == 2010)

# How many observations are in the training set?

# Let's build a logistic regression model
# We will only include numerical attributes of the song
exclude = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[!(names(train) %in% exclude)]
test = test[!(names(test) %in% exclude)]

# 1.2.2 - Now build the model using all variables - what is the Akaike Information Criterion
mod1 = glm(Top10 ~ ., family = "binomial", data = train)
summary(mod1)

# 1.2.3 - What does our model suggest about related to the confidence in time signature, key, and tempo?
  # The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10

# 1.2.4 - In general, if the confidence is low for the time signature, tempo, and key, the the song is more likely to be complex?  What does mod1 suggest in terms of complexity?
  # Mainstream listeners perform less complex songs

# 1.2.5 - Looking at the coefficient on "loudness", what does mod1 suggest?
  # Mainstream listeners prefer songs with heavy instrumentation
  
  # Can we draw the same conclusions about "energy"
    # No

# We must be aware of Multicollinearity!!
# 1.3.1 - What is the correlation between "loudness" and "energy"?
cor(song$loudness, song$energy)

# Create a new model, mod2, removing "loudness" from mod1
# Below notice "-loudness", this only works for numeric variables
mod2 = glm(Top10 ~ .-loudness, family = "binomial", data = train)

# 1.3.2 - What do you observe about the coefficient on the variable "energy" now?
summary(mod2)

# 1.3.3 - Now repeat the process from 1.3.2, but remove "energy instead"
# Do we make the same observation about loudness?
mod3 = glm(Top10 ~ .-energy, family = "binomial", data = train)
summary(mod3)

# 1.4.1 - What is the accuracy of mod3 on the test set, using a threshold of 0.45?
pred = predict(mod3, type = "response", newdata = test)
conf_mat_1 = table(test$Top10, pred >= 0.45)

# Accuracy = (TN + TP) / Number of observations
(conf_mat_1[1,1] + conf_mat_1[2,2]) / nrow(test)

# 1.4.2 - What is the accuracy of the baseline model on the test set?
baseline_table = table(test$Top10)
baseline_table[1] / sum(baseline_table[])

# 1.4.3 - How many songs does mod3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
#       - How many non-hit songs does mod3 predict will be Top 10 hits
conf_mat_1

# 1.4.4 - What is the sensitivy of mod3?
#       - What is the specificity of mod3?
conf_mat_1[2,2] / sum(conf_mat_1[2,])  # Sensitivity = TP / (TP + FN)
conf_mat_1[1,1] / sum(conf_mat_1[1,])  # Specificity = TN / (TN + FP)

# So, does our model offer us a competitive edge?
# Yes, our model provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely.
# So while it detects less than half of the Top 10 songs, we can be very confident in the songs that does predict to be Top 10 hits.

#############################################
# Problem set 2 - Predicting parole violators
#############################################

#############################################
#
# Colum guide
#
# male              = 1 if the parolee is male, 0 if female
# race              = 1 if the parolee is white, 2 otherwise
# age               = the parolee's age (in years) when he or she was released from prison
# state             = a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served       = the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence      = the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses = 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime             = a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator          = 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.
#
#############################################

# clear some space
rm( list = ls() )

# read in the data
parole = read.csv('parole.csv')

# 2.1.1 - How many observations are in the dataset?
summary(parole)

# 2.1.2 - How many parolees violated the terms of their parole?
table(parole$violator)

# 2.2.1 - Which variables in this dataset are unordered factors with at least three levels?
# Use the column guide to help make the determination

# 2.2.2 - How does the output of summary() change for a factor variable as compared to a numeric variable?
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole)

# 2.3.1 - Roughly what proportion of parolees have been allocated to the training and test sets?
library(caTools)
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# 2.3.2 - Three questions related to re-running the code above - what happens to the output of the data when you vary the set.seed()?

# 2.4.1 - train a logistic regression model on the training set, which variables are significant?
mod1 = glm(violator ~ ., family = "binomial", data = train)
summary(mod1)

# 2.4.2 - What can we say based on the coefficient of the multiple.offenses variable?
# Recall that:
# 1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit incrase in the variable.
# 2) If we have a coefficient c for a variable, then tha tmeans the odds are multiplied by exp(c) for a unit incrase in the variable.

exp(mod1$coefficients["multiple.offenses"])

# 2.4.3 - According to the model, what are the odds this individual is a violator?
# male, white, 50 years old, from Maryland, served 3 months, max sentence = 12 months, did not commit multiple offenses, and committed larceny
i = mod1$coefficients["(Intercept)"]*1
m = mod1$coefficients["male"]*1
w = mod1$coefficients["race"]*1
a = mod1$coefficients["age"]*50
s = mod1$coefficients["max.sentence"]*12
t = mod1$coefficients["time.served"]*3
o = mod1$coefficients["multiple.offenses"]*0
l = mod1$coefficients["crime2"]*1

coefs = i+m+w+a+s+t+o+l       # sum these together
odds = exp(coefs)             # find the odds ratio
prob = 1 / (1 + exp(-coefs))  # calculate the probability

# 2.5.1 - Obtain the mod1 predicted probabilities on the test set.  What is the max predicted probability of a violation?
prediction = predict(mod1, type = "response", newdata = test)
max(prediction)

# 2.5.2 - Evaluate the model's predictions on the test set using a 0.5 threshold
# What is the model's sensititivy, specificity, and accuracy?
conf_mat = table(test$violator, prediction >= 0.5)
conf_mat

conf_mat[2,2] / sum(conf_mat[2,]) # Sensitivity = TP / (TP + FN)
conf_mat[1,1] / sum(conf_mat[1,]) # Specificity = TN / (TN + FP)
(conf_mat[1,1] + conf_mat[2,2]) / nrow(test) # Accuracy = (TN + TP) / Number of observations

# 2.5.3 - What is the accuracy of a simple model that predicts that every parolee is a non-violator?
simple = table(test$violator)
simple[1] / nrow(test)

# 2.5.4
# Consider a parole board using the model to predict whether parolees will be violators or not.
# The job of a parole board is to make sure that a prisoner is ready to be released into free society,
# and therefore parole boards tend to be particularily concerned about releasing prisoners who will violate their parole.
# Which of the following most likely describes their preferences and best course of action?

# The board assigns more cost to a false negative than a false positive.  This is because a negative prediction would lead
# to a prisoner being granted parole, while a positive prediction would lead to a prisoner being denied parole.
# The parole board would experience more regret for releasing a prisoner who then violates parole than it would experience
# for denying parole to a prisoner who would not have violated parole.

# Recall, the threshold and specificity are positively correlated, so if I want to decrease the specificity (decrease the false negatives),
# then I would want to decrease the threshold.

# 2.5.5
# Our model is likely of value to the board because our model produces 12 FPs and 11 FNs vs the simple model of 0 FPs and 23 FNs.
# Because board is likely keen on FNs, decreasing the threshold would improve the models values to the board

# 2.5.6 - Using the ROCR package, what is the AUC for the model?
library(ROCR)
ROCRpred = prediction(prediction, test$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# 2.5.7
# Recall, the AUC tells us the probability the model can correctly differentiate between a
# randomly selected parole violator and a randomly selected non-parole violator
# The value produced by this model is quite good!

# 2.6.1
# The dataset contains all individuals released from parole in 2004, either due to completing their parole term or violating the terms of their parole.
# However, it does not contain parolees who neither violated their parole nor completed their term in 2004, causing non-violators to be underrepresented.
# This is called "selection bias" or "selecting on the dependent variable," because only a subset of all relevant parolees were included in our analysis,
# based on our dependent variable in this analysis (parole violation). How could we improve our dataset to best address selection bias?

# We should use a dataset tracking a group of parolees from the start of their parole unitl either they violated parole or they completed their term.
# As a result, a prospective dataset that tracks a cohort of parolees and observes the true outcome of each is more desirable.
# Unfortunately , such datasets are often more challenging to obtain (for instance, if a parolee had a 10-year term, it might require tracking that individual
# for 10 years before building the model).  Such a prospective analysis would not be possible using the 2004 National Corrections Reproting Program dataset.

############################################
# Problem set 3 - Predicting loan repayment
############################################

############################################
#
# Column guide
#
# credit.policy     = 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
# purpose           = The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
# int.rate          = The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
# installment       = The monthly installments ($) owed by the borrower if the loan is funded.
# log.annual.inc    = The natural log of the self-reported annual income of the borrower.
# dti               = The debt-to-income ratio of the borrower (amount of debt divided by annual income).
# fico              = The FICO credit score of the borrower.
# days.with.cr.line = The number of days the borrower has had a credit line.
# revol.bal         = The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
# revol.util        = The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
# inq.last.6mths    = The borrower's number of inquiries by creditors in the last 6 months.
# delinq.2yrs       = The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
# pub.rec           = The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).
# not_full_paid     = indicates if the lona was not paid back in full
#
############################################

# clear some space
rm( list = ls() )

# read in the data
loans = read.csv('loans.csv')

# 3.1.1 - What proportion of the loans in the dataset were not paid in full?
# 3.1.2 - Which variables have at least one missing observation?
summary(loans)

# 3.1.3 - In preparing the dataset, we want to be able to predict risk for all borrowers, instead of just the ones with all data reported.
#       - for this reason, we will not remove the missing observations, because their paid_in_full rate is not substantially different from the rest of our data

# 3.1.4 - Pepare the dataset
library(mice)
set.seed(14)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# For our imputation, we predicted missing variables using the available independent variables for each observation.

# Split the data into a training and a test set
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

# 3.2.1 - Now build a logistic regression model trained on the training set to predict not.fully.paid using all independent variables
mod1 = glm(not.fully.paid ~ ., family = "binomial", data = train)
summary(mod1)

# 3.2.2 - Consider two loan applicants, A & B, who are identical except for applicant A has a FICO of 700 while B has a FICO of 710.
# What Logit(A) - Logit(B) - the log odds of A - the log odds of B
mod1$coefficients["fico"]*10

# What is the odds of A / odds of B?
# Using some algebra, I know that this will be the same code as above, but * -10 (when dividing exponents, you take the difference of their polynomial)
exp(mod1$coefficients["fico"]*-10)

# 3.2.3 - Predict the probability of the test set loans not being paid back in full
pred = predict(mod1, type = "response", newdata = test)

# Store these predicted probabilities as a new variable in the test set
test$predicted.risk = pred

# What is the accuracy of the logistic regression model?
conf_mat = table(test$not.fully.paid, pred >= 0.5)
accuracy = (conf_mat[1,1] + conf_mat[2,2]) / nrow(test)
accuracy

# What is the accuracy of the baseline model?
base_conf_mat = table(test$not.fully.paid)
base_accuracy = base_conf_mat[1] / nrow(test)
base_accuracy

# 3.2.4 - Use the ROCR package to compute the test set AUC
library(ROCR)
ROCRpred = prediction(pred, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# 3.3.1 - Build a model using only int.rate.  What could be causing the difference in the significance in this model vs our previous model?
mod2 = glm(not.fully.paid ~ int.rate, family = "binomial", data = train)
summary(mod2)
summary(mod1)

# 3.3.2 - Make test set predictions for the bivariate model.
# What is the highest predicted probability of a loan not being paid in full on the testing set?
bi_pred = predict(mod2, type = "response", newdata = test)
max(bi_pred)

# Using a threshold of 0.5, how many loans would be predicted as not being paid in full in the testing set?
table(test$not.fully.paid, bi_pred > 0.4266)

# 3.3.3 - What is the test set AUC of the bivariate model?
ROCRbi_pred = prediction(bi_pred, test$not.fully.paid)
auc_bi = as.numeric(performance(ROCRbi_pred, "auc")@y.values)
auc_bi

# 3.4.1 - How much does a $10 investment with an annual interest rate of 6% pay back after three years, using continuous compounding interest?
c = 10; i = 0.06; t = 3
payback = c*exp(i*t)

# 3.4.2 & 3.4.3 are theory questions and didn't require any code

# 3.5.1
# In ordr to evaluate the quality of an investment strategy, we need to compute the profit for each loan in the test set.
# Let's create a new variable, assuming a $1 investment, to make this assessment
test$profit = exp(test$int.rate*3) - 1 # the dataset contains 3-year loans, so t = 3

# Update profit column to be -1 for all loans that didn't fully repay (this is assuming we gained nothing back from the loan)
test$profit[test$not.fully.paid == 1] = -1

# What is the max profit of a $10 investment in any loan on the test set?
max_profit = max(test$profit) * 10
max_profit

# 3.6.1 - We will now analyze an investment strategy in which the investor only purchases loans with a high interest rate (>=.15),
# but amongst these lonas selects the ones with the lowest predicted risk of not being paid back in full.
# Assume an investor invests $1 in each of the most promising 100 loans.

high_i = subset(test, int.rate >= 0.15)

# What is the average profit of these high interest loans?
avg_prof_high_i = mean(high_i$profit)
avg_prof_high_i

# What proportion of the high-interest loans were not paid back in full?
table(high_i$not.fully.paid)[2] / nrow(high_i)

# 3.6.2 - Find the 100th smallest predicted probability of not paying in full
cutoff = sort(high_i$predicted.risk, decreasing = FALSE)[100]

# Now build a dataset of the highest interest loaons with predicted risk not exceeding the cutoff we just computed
selected_loans = subset(high_i, predicted.risk <= cutoff)

# What is the profit of the investor, who invested $1 in each of these 100 loans?
sum(selected_loans$profit)

# How many of the 100 selected loans were not paid back in full?
table(selected_loans$not.fully.paid)[2] / nrow(selected_loans)

# We conclude with a note of warning. Throughout this analysis we assume that the loans we invest in will perform in the same way as the loans
# we used to train our model, even though our training set covers a relatively short period of time. If there is an economic shock like a large
# financial downturn, default rates might be significantly higher than those observed in the training set and we might end up losing money instead
# of profiting. Investors must pay careful attention to such risk when making investment decisions.