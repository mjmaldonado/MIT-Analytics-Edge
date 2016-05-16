####################################
#
# Week 3 Recitation: Using Regression Trees on Housing data
#
# Author: Michael Maldonado
#
####################################

####################################
#
# Column guide
#
# tract         = a statistical division of the are that is used by researchers to break down towns and cities
# LON and LAT   = the longitude and latitude of the center of the cenus tract
# MEDV          = the median value of owner-occupied homes, in thousands of dollars
# CRIM          = the per capita crime rate
# ZN            = how much of the land is zoned for large residential properties
# INDUS         = proportion of area used for industry
# CHAS          = 1 if census tract is next to the Charles River
# NOX           = concentration of nitrous oxides in the air
# RM            = the average number of rooms per dwelling
# AGE           = propoortion of owner-occupied units built before 1940
# DIS           = a measure of how the tract is from centers of employment from Boston
# RAD           = a measure of closeness to important highways
# TAX           = the property tax rate per $10,000 of value
# PTRATIO       = the pupil-teacher ratio by town
#
####################################

# clear some space
rm( list = ls() )

# read in the data
boston = read.csv('boston.csv')

# take a look at the data
str(boston)

# our original model is to see how prices vary across region
# let's take a look at our map
plot(boston$LON, boston$LAT)

# Let's take a look at which of these points falls on the Charles River
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = 'blue', pch = 19)

# How about MIT?
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = 'red', pch = 19)

# How about the points with above average pollution?
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col = 'green', pch = 19)

# How do prices vary?
plot(boston$LON, boston$LAT)

# Let's get some summary stats on our prices
summary(boston$MEDV)
# Let's look at all points with values greater than the price median
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = 'red', pch = 19)

# Let's see if a Linear regression might be a good model to choose
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

# Not looking good, but we'll try a linear regression anyways
boston_lm = lm(MEDV ~ LAT + LON, data = boston)
summary(boston_lm)

# Let's get a MEDV plot back
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = 'red', pch = 19)

# What does a linear regression model think is higher than the median value?
points(boston$LON[boston_lm$fitted.values >= 21.2], boston$LAT[boston_lm$fitted.values >= 21.2], col = 'blue', pch = '$')

# You can see the sharp line the linear regression model defines.  You can see it's not a very good predictor of house prices

# Let's see how this compares to Regression Trees
library(rpart)
library(rpart.plot)

boston_CART = rpart(MEDV ~ LAT + LON, data = boston)
prp(boston_CART)

# build our past plot again
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = 'red', pch = 19)

fittedValues = predict(boston_CART)
points(boston$LON[fittedValues >= 21.2], boston$LAT[fittedValues >= 21.2], col = 'blue', pch = '$')

# We can see that the CART model is a much better predictor of prices as a function of location than the linear regression model

# Let's see if we can get a similar prediction with a similar tree
boston_CART = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(boston_CART)
text(boston_CART)

plot(boston$LON, boston$LAT)
abline(v = -71.07) # the first tree split
abline(h = 42.21) # the lowest median price
abline(h = 42.17) # this essentially outline the Charles River
# Let's re-plot the points >= the median house price
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = 'red', pch = 19)

# So, the CART model has helped us carve out the lowest priced houses
# Now we need to determine if the CART model will be useful in helping us predict house prices

# Let's split our data into a test and training set
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

# Let's try a linear regression model first
boston_lm = lm(MEDV ~ 
                 LAT + 
                 LON + 
                 CRIM + 
                 ZN + 
                 INDUS + 
                 CHAS + 
                 NOX + 
                 RM + 
                 AGE + 
                 DIS + 
                 RAD + 
                 TAX + 
                 PTRATIO, 
               data = train
               )
summary(boston_lm)

boston_lm_pred = predict(boston_lm, newdata = test)
boston_lm_sse = sum((boston_lm_pred - test$MEDV)^2)
boston_lm_sse

# Now let's try a CART model
boston_CART = rpart(MEDV ~ 
                 LAT + 
                 LON + 
                 CRIM + 
                 ZN + 
                 INDUS + 
                 CHAS + 
                 NOX + 
                 RM + 
                 AGE + 
                 DIS + 
                 RAD + 
                 TAX + 
                 PTRATIO, 
               data = train
)
prp(boston_CART)

# How does this compare to the linear model?
boston_CART_pred = predict(boston_CART, newdata = test)
CART_sse = sum((boston_CART_pred - test$MEDV)^2)
CART_sse

# It show that the linear regression actually performs better at predicting hosue prices than the CART model

# Now let's revisit the "cp" parameter and see if we can use Cross-Validatoin to improve the perfomance of our CART model
# "cp" = "complexity parameter"
# recall the first tree we made using LAT/LON had many splits, but we were able to trim it without losing much accuracy.

# Intuition: too many splits = bad generalization, so we should penalize the complexity
# Smaller values of cp encourage large trees (less penalty) and larger values of cp encourage small trees (large penalty)

library(caret)
library(e1071)

tr.control = trainControl(method="cv", number = 10) # 10-fold cross-validation
cp.grid = expand.grid(.cp = seq(0,0.01,.001)) # our grid of cp's to search over

tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

# We get a value extremly close to 0 - this implies it want us to use a higher complexity model
best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse

# How does this compare to the linear regression, now that we've used cross-validation to improve our model?
boston_lm_sse

# Well, the linear regression still prevails, but it was worth a shot!
