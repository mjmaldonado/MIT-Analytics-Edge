###########################
# Week 2 problem set
#
# Author: Michael Maldonado
#
###########################

CC = read.csv('climate_change.csv')

str(CC)

# Year     = the observation year
# Month    = the observation month
# Temp     = the difference in degrees Celsius between the average
#            global temperature in that period and a reference value.
# CO2      = atmospheric concentration of carbon dioxide
# N20      = atmospheric concentration of nitrous oxide
# CH4      = atmospheric concentration of methane
# CFC.11   = atmospheric concentration of trichlorofluoromethane
# CFC.12   = atmospheric concentration of dichlorodifluoromehane
# Aerosols = the mean stratospheric optial depth at 550 nm.
#            this variable is linked to volcanoes, as volcanic eruptions
#            result in new particles being added to the atmosphere
#            which affect how much of the sun's energy is reflected back into
#            space.
# TSI      = the total solar irradience (TSI) in W/m^2 (the rate at which the
#            sun's energy is deposited per unit area).  Due to sunspots and other
#            solar phenomena, the amount of energy that is given off by the sun
#            varies substantially with time.
# MEI      = multivariate El Nino Souther Oscillation index (MEI), a measure of
#            the strength of the EL Nino/La Nina-Southern Oscillation (a weather effect
#            in the Pacific Ocean that affects global temperatures).

#####################
# Problem set 1
#####################

# 1.1.1
# Split the data into a training and test set
train = subset( CC, Year < 2007 )
test  = subset( CC, Year >= 2007 )

# 1.1.2
# Predict temperature based on atmospheric variables
reg = lm( Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train )
summary(reg)

# 1.2.1
# We can observe that CFC-11 and N20 have negative coefficients - this implys collinarity
# between these variables and other variables.  Let's take a look to see which variables these are.
cor(train)

# 1.2.2
reg2 = lm( Temp ~ MEI + TSI + Aerosols + N2O, data = train )
summary(reg2)

# 1.3 & 1.4
# Let's have R help us choose a model with the lowest calculated Akaike information criterion (AIC)
# Essentially, choose the highest quality model (relatively speaking), with the least amount of lost information
reg_step = step(reg)
summary(reg_step)

# It's interesting to note that the step functino does not address collinearity of the variables,
# except that adding highly correlated variables will not improve the R squared significantly.
# The consequence of this is that the step function will not necessarily produce a very interpretable model
# just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC)

# 1.5
# Now let's see how well our model performs on new data
test_prediction = predict(reg_step, newdata = test)
summary(test_prediction)

SSE = sum( (test_prediction - test$Temp)^2 )
SST = sum( (mean(train$Temp) - test$Temp)^2 )
R2 = 1 - (SSE / SST)
R2

# Clear some space for the next problem set
rm( list = ls() )


#####################
# Problem set 2
#####################


###################################
# Column definitions
#
# grade                 = The grade in school of the student (most 15-year-olds in America are in 10th grade)
# male                  = Whether the student is male (1/0)
# raceeth               = The race/ethnicity composite of the student
# preschool             = Whether the student attended preschool (1/0)
# expectBachelors       = Whether the student expects to obtain a bachelor's degree (1/0)
# motherHS              = Whether the student's mother completed high school (1/0)
# motherBachelors       = Whether the student's mother obtained a bachelor's degree (1/0)
# motherWork            = Whether the student's mother has part-time or full-time work (1/0)
# fatherHS              = Whether the student's father completed high school (1/0)
# fatherBachelors       = Whether the student's father obtained a bachelor's degree (1/0)
# fatherWork            = Whether the student's father has part-time or full-time work (1/0)
# selfBornUS            = Whether the student was born in the United States of America (1/0)
# motherBornUS          = Whether the student's mother was born in the United States of America (1/0)
# fatherBornUS          = Whether the student's father was born in the United States of America (1/0)
# englishAtHome         = Whether the student speaks English at home (1/0)
# computerForSchoolwork = Whether the student has access to a computer for schoolwork (1/0)
# read30MinsADay        = Whether the student reads for pleasure for 30 minutes/day (1/0)
# minutesPerWeekEnglish = The number of minutes per week the student spend in English class
# studentsInEnglish     = The number of students in this student's English class at school
# schoolHasLibrary      = Whether this student's school has a library (1/0)
# publicSchool          = Whether this student attends a public school (1/0)
# urban                 = Whether this student's school is in an urban area (1/0)
# schoolSize            = The number of students in this student's school
# readingScore          = The student's reading score, on a 1000-point scale
#
###################################

# Read in the datasets
pisa_train = read.csv('pisa2009train.csv')
pisa_test = read.csv('pisa2009test.csv')

# 2.1.1 - How many students are in the training set?
nrow(pisa_train)

# 2.1.2 - What is the average test score of males and of females?
tapply(pisa_train$readingScore, pisa_train$male, mean)

# 2.1.3 - Which variables are missing data in at least one observation in the training set?
summary( pisa_train )

# 2.1.4 - Remove missing values - how many observations are now in each dataset?
pisa_train = na.omit(pisa_train)
pisa_test = na.omit(pisa_test)

str(pisa_train); str(pisa_test)

# 2.2.1
# Which variables are unordered factors with at least 3 levels?
# Which variables are ordered factors with at least 3 levels?
# raceeth; grade

# 2.2.2 - Which binary variables will be included in the regression model?
# All except raceethWhite because it is the reference level factor

# 2.2.3
# For an asian student, which variables will be set to 0?
# For a white student, which variables will be set to 0?
# All variables except for Asian; All variables

# 2.3.1
# By default R will select the reference level alphabetically, but we would like
# our reference level to be "White" because it is the most commonly occurring raceeth
# in the dataset

pisa_train$raceeth = relevel(pisa_train$raceeth, "White")
pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

# What is the R squared?
reg = lm( readingScore ~ ., data = pisa_train )
summary( reg )

# Notice the R squared is lower than in previous models.  This does not necessarily
# imply that the model is of poor quality.  More often than not, it simply means that the
# prediction problem at hand is more difficult than other prediction problems.

# 2.3.2
# What is the RMSE of the regression?
SSE_train = sum( (reg$residuals)^2 )
RMSE_train = sqrt( SSE_train / nrow(pisa_train) )
RMSE_train

# 2.3.3
# Student A is in grade 11; Student B is in grade 9.  What is the difference in their predicted reading scores?
grade_diff = reg$coefficients[2]*11 - reg$coefficients[2]*9
grade_diff

# 2.3.4 - What is the meaning of the coefficient associated with variable raceethAsian
reg$coefficients["raceethAsian"]

# The predicted difference in reading score between as Asian student and a white student who is otherwise identical.

# 2.3.5 - Based on the significance codes, which are candidates for removal from the model?
summary(reg)

# 2.4.1 - What is the range between the maximum and minimum predicted reading score on the test set?
pred = predict(reg, newdata = pisa_test)
summary(pred)
max(pred) - min(pred)

# 2.4.2 - What is the sum of squared errors (SSE) of reg on the testing set?
SSE_pred = sum( (pred - pisa_test$readingScore)^2 )
SSE_pred

RMSE_pred = sqrt( (SSE_pred) / nrow(pisa_test) )
RMSE_pred

# 2.4.3
# What is the predicted test score used in the baseline model?
base_test_score = mean(pisa_train$readingScore)
base_test_score

# What is the sum of squared errors of the baseline model on the testing set?
SST = sum( (base_test_score - pisa_test$readingScore)^2 )
SST

# 2.4.4 - What is the R-squared value of reg?
R2 = 1 - (SSE_pred/SST)
R2

# Clear some space for the next problem set
rm( list = ls() )

#####################
# Problem set 3
#####################

Flu_tr = read.csv('FluTrain.csv')

###################################
# Column guide
#
# Week    = The range of dates represented by this observation, in year/month/day format
# ILI     = This column lists the percentage of ILI-reltaed physician visits for the corresponding week
# Queries = This column lists the fraction of queries that are ILI-related for the corresponding week,
#           adjusted to be between 0 and 1 (higher values correspond to more ILI search queries)
#
###################################

# 3.1.1
# Which week corresponds to the highest percentage of ILI-related physician visit?
Flu_tr$Week[which.max(Flu_tr$ILI)]

# Which week corresponds to the highest percentage of ILI-related query fraction?
Flu_tr$Week[which.max(Flu_tr$Queries)]

# 3.1.2 - Plot the histogram of the dependent variable, ILI.  What best describes the distribution of values?
hist(Flu_tr$ILI)

# 3.1.3 - Plot the natural logarithm of ILI versus Queries.  What does this plot suggest?
plot( log(Flu_tr$ILI), Flu_tr$Queries )

# 3.2.1 - Based on this plot, what is the most reasonable linear regression model?
Flu_1 = lm( log(ILI) ~ Queries, data = Flu_tr )

# 3.2.2 - What is the training set R-squared value for Flu_1 model
summary(Flu_1)

# 3.2.3 - There is a direct relationship between the R-squared and the correlation between the independent and dependent variables.
# What is the relationship we infer from our problem?
correlation = (cor(Flu_tr$Queries, log(Flu_tr$ILI)))

cor2 = (correlation)^2
inv_log_cor = log( 1 / correlation)
exp_cor = exp(-0.5 * correlation)

# Now check this against the value of R-squared
cor2; inv_log_cor; exp_cor

# 3.3.1 - Read in FluTest.csv
Flu_test = read.csv('FluTest.csv')

# How does our data perform on new data?
# Because our model is in terms of log(ILI), we need to convert this back to normal ILI in the prediction.
Flu_pred = exp( predict(Flu_1, newdata = Flu_test) )

# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
Flu_pred[which(Flu_test$Week == "2012-03-11 - 2012-03-17")]

# 3.3.2 - What is the relative error between the estimate (our prediction) and the observed value for the week of March 11, 2012?
index = which(Flu_test$Week == "2012-03-11 - 2012-03-17")

( Flu_test$ILI[index] - Flu_pred[index] ) / Flu_test$ILI[index]

# 3.3.3 - What is the RMSE between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
RMSE_pred = sqrt( mean((Flu_pred - Flu_test$ILI)^2) )
RMSE_pred

# Training a Time Series Model
install.packages('zoo')
library(zoo)

# Create a variable which is lagged two weeks, as this is when the most recent data exists
ILILag2 = lag( zoo(Flu_tr$ILI), -2, na.pad = TRUE )
Flu_tr$ILILag2 = coredata(ILILag2)

# Add this variable to the test set, as well.
ILILag2 = lag( zoo(Flu_test$ILI), -2, na.pad = TRUE )
Flu_test$ILILag2 = coredata(ILILag2)

# 3.4.1 - How many missing values exist in the new variable?
summary(Flu_tr$ILILag2)

# 3.4.2 - Plot log(ILILag2), log(ILI).  Which best describes the relationship between these two variables?
plot( log(Flu_tr$ILILag2), log(Flu_tr$ILI) )

# 3.4.3
# Train a linear regression model on the Flu_tr dataset to predict the log of ILI using:
# Queries + log(ILILag2)
# Which coefficients are significant at the p=0.05 level?
# What is the R-squared value?
Flu_2 = lm( log(ILI) ~ Queries + log(ILILag2), data = Flu_tr )
summary(Flu_2)

# 3.4.4 - On the basis of R-squared and significance of coefficients, which statement is more accurate?

# 1) Due to overfitting, Flu_2 is a weaker model than Flu_1 on the training set
# 2) Flu_2 is about the same quality as Flu_1 on the training set
# 3) Flu_2 is a stronger model than Flu_1 on the training set

summary(Flu_1); summary(Flu_2)
# 3) because R-squared is significantly improved from Flu_1 to Flu_2, and log(ILILag2) is highly signficiant.
#    As a result, there is no sign of overfitting, and Flu_2 is superior to Flu_1 on the training set

# 3.5.1 - How many missing values are there in the test set ILILag2?
summary(Flu_test$ILILag2)

# 3.5.2 - Which value should be used to fill in the ILILag2 variable for the first observation in FluTest
tail(Flu_tr); head(Flu_test)

# 3.5.3
# Fill in the missing values of ILILag2 in the test set
Flu_test$ILILag2[1] = Flu_tr$ILI[416]
Flu_test$ILILag2[2] = Flu_tr$ILI[417]

# What is the new value of ILILag2 in the first row of the test set?
# What is the new value of ILILag2 in the second row of the test set?
head(Flu_test)

# 3.5.4
# Obtain test set predictions of the ILI variable from the Flu_2 model.
Flu_pred2 = exp( predict( Flu_2, newdata = Flu_test) )

# What is the test-set RMSE of Flu_2?
RMSE_pred2 = sqrt( mean((Flu_pred2 - Flu_test$ILI)^2) )
RMSE_pred2

# Which model obtained the best test-set RMSE?
RMSE_pred; RMSE_pred2
