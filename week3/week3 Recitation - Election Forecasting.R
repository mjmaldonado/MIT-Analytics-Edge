####################################################################
#
# Election Forecasting to predict a likely winner using polling data
#
# Author: Michael Maldonado
#
####################################################################

# This United States have 50 states
# Each state is assigned a number of electoral votes based on population

### Most votes: 55 (California)
### Least votes: 3 (Multiple states)
### Reassigned periodically based on population change

# A "winner-takes-all" system is used in most states, but there are a few exceptions
# in which electoral votes are split according to individual voting totals.

# A candidate needs at least 270 electoral votes to win the presidential election.
# If more than two candidates receive electoral votes, it is possible that the candidate
# with the most electoral votes could fall short of a majority and there would be no winner.

# clear any old info from previous work
rm( list = ls() )

# Load the data
poll = read.csv('PollingData.csv')

# Take a look at our data
str(poll)

#####################################
#
# Column guide
#
# Year        = Election year
# Rasmussen   = People polled (as a percentage) who said they would like vote Republican - likely Democractic voters
# SurveyUSA   = Same as Rasmussen
# DiffCount   = Polls with Republican winner - Polls with Democratic winner
# PropR       = Polls with Republican winner divided by the number of polls
# Republican  = Outcome variable, 1 = Republican; 0 = Democrat
#
#####################################

# It appears we have some missing data
summary(poll)

# How can we handle this missing data?
  # Delete missing observations
  # Delete variables with missing values
  # Fill missing data points with average values

# Thankfully there is a package out there that can help us with imputation
install.packages('mice')
library(mice)

simple = poll[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

set.seed(144)

# Grab dplyr, a nice package for data manipulation
install.packages('dplyr')
library(dplyr)

# Now let's impute values for our missing poll data
imputed = simple %>% mice %>% complete()

# The output shows us that five rounds of imputation have been completed
# And there are no more missing values
summary(imputed)

# Now the last step is to get this data back into our original dataset
poll$Rasmussen = imputed$Rasmussen
poll$SurveyUSA = imputed$SurveyUSA

# Let's check to make sure we have not more missing values
summary(poll) # Success!

# Now let's make a model

# Make test set and a training set
train = subset(poll, Year < 2012)
test = subset(poll, Year == 2012)

# What is our baseline model?
table(train$Republican)

# That a republican will win a state 53% of the time.  Unforunately this is a pretty weak model.
# So, we need to think of a smarter baseline model.

# Let's make a baseline based on which party is polling higher in that particular state, and inconclusive otherwise
train$Rasmussen %>% sign %>% table

# Let's use the training sets outcome against the sign of the polling data
table(train$Republican, sign(train$Rasmussen))

# Let's check for multicollinearity prior to building our model
cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# From this, we can stand to reason that it might make sense to build our model slowly.
# Let's use a univariate method to begin with (from the cor() we see that PropR has the highest correlation with Republican)
mod1 = glm( Republican ~ PropR, family = binomial, data = train )
summary(mod1)

# Let's see how this model does on the train set
pred1 = predict(mod1, type = "response")
table(train$Republican, pred1 >= 0.5)

# We can see that on our training set, the model only makes four mistakes (this is about the same as our baseline model)

# Now let's try a multivariate model to see if we can improve the performance of our model
mod2 = glm(Republican ~ SurveyUSA + DiffCount, family = binomial, data = train)
pred2 = predict(mod2, type = "response")
table(train$Republican, pred2 >= 0.5)

# We improved our errors by 1 by including 1 more variable.
# Let's take a look at the summary of our model
summary(mod2)

# We can see that our second model produces less errors on our training set, but the variables are much less significant than the univariate model
# Let's see how our models perform on the test set

# Let's first obtain our baseline
table(test$Republican, sign(test$Rasmussen))

test_prediction = predict(mod2, newdata = test, type = "response")
table(test$Republican, test_prediction >= 0.5)

# For all but one, we have predicted correctly.  In this particular case, we aren't necessarily concerned with any one type of error, so we are OK with using a 0.5 threshold

# Let's investigate the error we made
subset(test, test_prediction >= 0.5 & Republican == 0)

# This result isn't too surprising, and this model is performing better than our smart baseline model so it would seem reasonable to use this for our logistic regression model