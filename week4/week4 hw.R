#########################################
#
# Week 3 HW
#
# Author: Michael Maldonado
#
#########################################

#########################################
#
# Problem Set 1 - Understanding Why People Vote
#
#########################################

#########################################
#
# Column guide
#
# civicduty     = group members were sent a letter that simply said "DO YOUR CIVIC DUTY - VOTE!"
# hawthorne     = group members were sent a letter that had the "Civit Duty" message plus the additional
#                 message "YOU ARE BEING STUPID" and they were informed that their voting behavior would be 
#                 examined by means of public records
# self          = received "Civic Duty" message as well as the recent voting record of everyone in that household
#                 and a message stating that another message would be sent after the election with update records.
# neignbors     = group members were given the same message as that for "Self" group, except the message not only had the household voting but also that of neighbors - maximizing social pressure
# control       = group members were not sent anything, and represented the typical voting situation.
# sex           = 0 = male, 1 = female
# yob           = year of birht
# voting        = 1 = voted, 0 = otherwise
#
#########################################

# clear some space
rm( list = ls() )

# read in the data
gerber = read.csv('gerber.csv')

# 1.1.1 - what proportion of people in this dataset voted in the election?
mean(gerber$voting)

# 1.1.2 - Which of the four "treatment groups" had the largest percentage of people who actually voted?
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# 1.1.3 - build a logistic regression model using the four treatment variables
#         Which coefficients are significant?
log_reg = glm(voting ~ 
                civicduty + 
                hawthorne + 
                self + 
                neighbors, 
              family = "binomial", 
              data = gerber
              )
summary(log_reg)

# 1.1.4 - using a threshold of 0.3, what is the accuracy of the logistic regression model?
gerber.log.reg.pred = predict(log_reg, type = "response")

conf.mat.30 = table(gerber$voting, gerber.log.reg.pred > 0.3)
accuracy.30 = (conf.mat.30[1,1] + conf.mat.30[2,2]) / nrow(gerber)

# 1.1.5 - what is the accuracy with a threshold of 0.5
conf.mat.50 = table(gerber$voting, gerber.log.reg.pred > 0.5)
accuracy.50 = (conf.mat.50[1]) / nrow(gerber)

# 1.1.6 - compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC.  What is happening here?
library(ROCR)
ROCRpred = prediction(gerber.log.reg.pred, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# Now let's see how a CART model would perform on this data
library(rpart)
library(rpart.plot)

# 1.2.1 - build a CART model and plot the tree - what happens, and if relevant, why?
gerber.CART = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(gerber.CART)

# 1.2.2 - now build the model with a very low cp
gerber.CART = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
prp(gerber.CART)

# 1.2.3 - Using only the CART tree plot, what fraction of "Civic Duty" people voted?
# Look at the bottom right split of civicduty

# 1.2.4 - make a new tree that inclues the "sex" variable with a cp = 0.0.  In the control group, which gender is more likely to vote?
gerber.CART2 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp = 0.0)
prp(gerber.CART2)

# 1.3.1 - in the control only tree, what is the absolut value of the difference in the predicted probability of voting between being in the control group vs being in a differnt group?
gerber.CART3 = rpart(voting ~ control, data = gerber, cp = 0.0)
gerber.CART4 = rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(gerber.CART3, digits = 6)
0.34 - 0.296638

# 1.3.2 - using the second tree, who is affected more by not being in the control group?
prp(gerber.CART4, digits = 6)
0.345818 - 0.334176

# 1.3.3 - Going back to logistic regression now, create a model using "sex" and "control".  Interpret the coefficient for "sex"
gerber.log2 = glm(voting ~ control + sex, family = "binomial", data = gerber)
summary(gerber.log2)

# 1.3.4
# The regression tree calculated the percentage voting exactly for every one of the four possibilities
# (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). Logistic regression has attempted to do the same,
# although it wasn't able to do as well because it can't consider exactly the joint possibility of being a women and in the control group.

# We can quantify this precisely. Create the following dataframe (this contains all of the possible values of sex and control),
# and evaluate your logistic regression using the predict function (where "LogModelSex" is the name of your logistic regression
# model that uses both control and sex):
  
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerber.log2, newdata=Possibilities, type="response")

# The four values in the results correspond to the four possibilities in the order they are stated above
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ).
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
# Give an answer with five numbers after the decimal point.
abs(0.290456 - 0.2908065)

# 1.3.5
# So the difference is not too big for this dataset, but it is there.
# We're going to add a new term to our logistic regression now, that is the combination of the "sex" and "control" variables
# - so if this new variable is 1, that means the person is a woman AND in the control group. We can do that with the following command:

gerber.log3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

# This coefficient is negative, so that means that a value of 1 in this variable decreases the chance of voting. This variable will have variable 1 if the person is a woman and in the control group.

# 1.3.6 - run the same code as before to calculate the average for each group
log3.predict = predict(gerber.log3, type = "response", newdate = Possibilities)
abs(0.2904558 - 0.290456)


###############################################
#
# Problem set 2 - Letter recognition
#
# Build a model that uses statistics of images of four letters
# in the Roman alphabet -- A, B, P, and R -- to predict which letter a particular
# image corresponds to.
#
###############################################

###############################################
#
# Column guide
#
# letter    = the letter that the image corresponds to (A, B, P or R)
# xbox      = the horizontal position of where the smallest box covering the letter shape begins.
# ybox      = the vertical position of where the smallest box covering the letter shape begins.
# width     = the width of this smallest box.
# height    = the height of this smallest box.
# onpix     = the total number of "on" pixels in the character image
# xbar      = the mean horizontal position of all of the "on" pixels
# ybar      = the mean vertical position of all of the "on" pixels
# x2bar     = the mean squared horizontal position of all of the "on" pixels in the image
# y2bar     = the mean squared vertical position of all of the "on" pixels in the image
# xybar     = the mean of the product of the horizontal and vertical position of all of the "on" pixels in the image
# x2ybar    = the mean of the product of the squared horizontal position and the vertical position of all of the "on" pixels
# xy2bar    = the mean of the product of the horizontal position and the squared vertical position of all of the "on" pixels
# xedge     = the mean number of edges (the number of times an "off" pixel is followed by an "on" pixel, or the image boundary is hit) as the image is scanned from left to right, along the whole vertical length of the image
# xedgeycor = the mean of the product of the number of horizontal edges at each vertical position and the vertical position
# yedge     = the mean number of edges as the images is scanned from top to bottom, along the whole horizontal length of the image
# yedgexcor = the mean of the product of the number of vertical edges at each horizontal position and the horizontal position
#
###############################################

# clear some space
rm( list = ls() )

# read in the data
letter.df = read.csv('letters_ABPR.csv')

# take a look at the data structure
str(letter.df)

# 2.1.1 - make a new variable isB to try and predict the letter B
letter.df$isB = as.factor(letter.df$letter == "B")

# make a training and test set
library(caTools)
set.seed(1000)
split = sample.split(letter.df$isB, SplitRatio = 0.50)
train = subset(letter.df, split == TRUE)
test = subset(letter.df, split == FALSE)

# consider a baseline model that always predicts the most frequent outcome "not  B" (isB == FALSE).
# what is the accuracy of this model on the test set?
accuracy.bl = table(test$isB)[1] / nrow(test)

# 2.1.2 - build a classification tree to predict whether a letter is B or not, using the training set to build the model.
library(rpart)
library(rpart.plot)

# make sure to remove "letter" out of the model because this is related to what we are trying to predict
train.CART = rpart( isB ~ .-letter, method = "class", data = train )

# what is the accuracy of train.CART on the test set?
predict.CART = predict( train.CART, newdata = test, type = "class" )
conf.mat.CART = table(test$isB, predict.CART)
accuracy.CART = (conf.mat.CART[1,1] + conf.mat.CART[2,2]) / nrow(test)

# 2.1.3 - now build a Random Forest model to predict whether the letter is B or not
library(randomForest)

set.seed(1000)
train.Forest = randomForest( isB ~ .-letter, method = "class", data = train )

# what is the accuracy of this model?
predict.Forest = predict( train.Forest, newdata = test, type = "class" )
conf.mat.Forest = table( test$isB, predict.Forest )
accuracy.Forest = (conf.mat.Forest[1,1] + conf.mat.Forest[2,2]) / nrow(test)

# It should be noted that random forests tend to improve on CART in terms of predictive accuracy.  Sometimes, this improvement can be quite significant, as it is here.

# 2.2.1 Let's move on to the original problem - predicting one of the four letters A, B, P, or R
# we need to first convert our dependent variable into a factor
letter.df$letter = as.factor(letter.df$letter)

# build a new test and training set
set.seed(2000)
split = sample.split(letter.df$letter, SplitRatio = 0.50)
train = subset(letter.df, split == TRUE)
test = subset(letter.df, split == FALSE)

# What is the baseline (the most frequent class) accuracy of this model?
accuracy.bl.letter = max(table(test$letter)) / nrow(test)

# 2.2.2 - Now build a CART model for letter (excluding isB)
# What is the test set accuracy of the CART model?
train.CART.letter = rpart( letter ~ .-isB, method = "class", data = train )
predict.CART.letter = predict( train.CART.letter, newdata = test, type = "class" )
conf.mat.CART.letter = table( test$letter, predict.CART.letter )
accuracy.CART.letter = (conf.mat.CART.letter[1,1] + conf.mat.CART.letter[2,2] + conf.mat.CART.letter[3,3] + conf.mat.CART.letter[4,4]) / nrow(test)

# 2.2.3 - Now build a Random Forest model on the training data
# What is the test set accuracy? (use a seed of 1000)
set.seed(1000)
train.Forest.letter = randomForest( letter ~ .-letter, method = "class", data = train )
predict.Forest.letter = predict( train.Forest.letter, newdata = test, type = "class" )
conf.mat.Forest.letter = table( test$letter, predict.Forest.letter )
accuracy.Forest.letter = (conf.mat.Forest.letter[1,1] + conf.mat.Forest.letter[2,2] + conf.mat.Forest.letter[3,3] + conf.mat.Forest.letter[4,4]) / nrow(test)

##########################################
# 
# Problem Set 3 - Predicting earnings from cenus data
#
# Let's use census information about an individual to predict how much a person earns
# In particular, does this person earn more than $50,000 per year
#
##########################################

##########################################
#
# Column guide
#
# age             = the age of the individual in years
# workclass       = the classification of the individual's working status (does the person work for the federal government, work for the local government, work without pay, and so on)
# education       = the level of education of the individual (e.g., 5th-6th grade, high school graduate, PhD, so on)
# maritalstatus   = the marital status of the individual
# occupation      = the type of work the individual does (e.g., administrative/clerical work, farming/fishing, sales and so on)
# relationship    = relationship of individual to his/her household
# race            = the individual's race
# sex             = the individual's sex
# capitalgain     = the capital gains of the individual in 1994 (from selling an asset such as a stock or bond for more than the original purchase price)
# capitalloss     = the capital losses of the individual in 1994 (from selling an asset such as a stock or bond for less than the original purchase price)
# hoursperweek    = the number of hours the individual works per week
# nativecountry   = the native country of the individual
# over50k         = whether or not the individual earned more than $50,000 in 1994
#
##########################################
# clear some space
rm( list = ls() )

# read the data
census = read.csv('census.csv')

# take a look at the structure of the dataset
str(census)

# 3.1.1 - let's start by building a logistic regression model to predict whether an individual earns more than $50,000 using all indpendent variables
# Split the data into a test set and a training set
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset( census, split == TRUE )
test = subset( census, split == FALSE )

# Now build the model
census.log.reg = glm( over50k ~., family = "binomial", data = train )
summary(census.log.reg)

# 3.1.2 - What is the accuracy of the model on the test set (using a threshold of 0.5)?
predict.log.reg = predict( census.log.reg, newdata = test )
conf.mat.log.reg = table( test$over50k, predict.log.reg > 0.5 )
accuracy.log.reg = (conf.mat.log.reg[1,1] + conf.mat.log.reg[2,2]) / nrow(test)

# 3.1.3 - What is the accuracy of the baseline model (the most frequently occuring) on the test set?
census.bl = max(table(test$over50k)) / nrow(test)

# 3.1.4 - What is the AUC for this model on the test set?
library(ROCR)
ROCRpred = prediction( predict.log.reg, test$over50k )
auc = as.numeric( performance( ROCRpred, "auc")@y.values )

# So, our logistic regression model for this data achieves high accuracy.
# Moreover, the significances of the variables give us a way to gauge which variables
# are relevant for this prediction task.  However, it is not immediately clear which variables
# are more important than the others, especially due to the large number of factor variables in this problem.
# For this reason, let's build a CART model to see if we can get similar accuracy with more interpretable results.

# 3.2.1 - How many splits does our tree have in total?
census.CART = rpart( over50k ~., method = "class", data = train )
prp(census.CART)

# 3.2.2 - Which variable does the tree split on at the first level?
# Take a look at the tree visual

# 3.2.3 - Which variables does the tree split on at the second level (immediately after the first split)?
# Take a look at the tree visual

# 3.2.4 - What is the accuracy of the model on the testing set (using a threshold of 0.5)?
predict.CART = predict( census.CART, newdata = test, type = "class" )
conf.mat.CART = table( test$over50k, predict.CART )
accuracy.CART = sum(diag(conf.mat.CART)) / nrow(test)

# 3.2.5 - Plot the ROC curve for the CART model.  Observe this compared to the log.reg ROC curve.
# The CART ROC is less smooth than the log.reg ROC, what's the reason for this?
predict.CART.ROC = predict( census.CART, newdata = test, type = "prob" )

ROCRpred.CART = prediction( predict.CART.ROC[,2], test$over50k )
ROCRperf = performance( ROCRpred, "tpr", "fpr" ) 
plot(ROCRperf, colorize = TRUE, main = "Receiver Operator Characteristic Curve", print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7)) 

auc.CART = as.numeric( performance(ROCRpred.CART, "auc")@y.values )

# The breakpoints of the curve correspond to the false and true positive rates when the threshold is set to the five possible probability values

# 3.3.1 - Build a random forest on a subset of the training set
set.seed(1)

train.Small = train[sample(nrow(train), 2000),]

set.seed(1)
census.Forest = randomForest( over50k ~., method = "class", data = train.Small )
predict.Forest = predict( census.Forest, newdata = test, type = "class" )
conf.mat.Forest = table( test$over50k, predict.Forest )
accuracy.Forest = sum(diag(conf.mat.Forest)) / nrow(test)

# 3.3.2
# Random Forest models work by building a large collection of trees and then takes an average of their results.
# Because of this, we lose some interpretability that comes with CART in terms of seeing how predictions are made
# and which variables are important.  However, we can still compute metrics that give us insights into which variables
# are important.

# One of these metrics looks at the number of times, aggregated over all trees in the random forest, that a certain variable
# is selected for a split.

# We can produce a chart that shows for each variable the number of times that variable was selected for splitting (value on the x-axis)
vu = varUsed(census.Forest, count = TRUE)
vu.sorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vu.sorted$x, names(census.Forest$forest$xlevels[vu.sorted$ix]))

# Which variable is the most important in terms of the number of splits?

# 3.3.3
# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is.
# In each tree in the forst, whenever we select a variable and perform a split, the impurity is decreased.  Therefore, one way to measure the importance
# of a variable is to average the reduction in impurity, taken over all the times that variable is selected for splitting in all of the trees in the forest.

varImpPlot(census.Forest)

# Which variable is the most important in terms of mean reduction in impurity?
# Take a look at the plot

# 3.4.1 - Selecting cp (the complexity parameter) by Cross-Validation
# Can we improve our CART model with cross-validation?
library(caret)
set.seed(2)
tr.control = trainControl(method = "cv", number = 10) #10-fold CV
CART.grid = expand.grid(.cp = seq( 0.002, 0.1, 0.002) ) # our grid of cp's to search over

census.CART.cv = train( over50k ~., 
                        data = train, 
                        method = "rpart", 
                        trControl = tr.control, 
                        tuneGrid = CART.grid
                      )

# Which value of cp does the train function recommend?
census.CART.cv

# 3.4.2 - Fit a CART model to the training data using the lowest cp.
# What is the prediction accuracy on the test set?
census.CART.cp = rpart( over50k ~., method = "class", data = train, cp = 0.002 )
predict.CART.cp = predict( census.CART.cp, newdata = test, type = "class" )
conf.mat.CART.cp = table( test$over50k, predict.CART.cp )
accuracy.CART.cp = sum(diag(conf.mat.CART.cp))/nrow(test)

# 3.4.3 - Compared to the original accuracy using the default value of cp, the new CART model is an improvement.
# So do we clearly favor this model over the first CART model?
# Take a look at the complexity of the new model.
prp(census.CART.cp)

# There are a significantly higher number of splits!
# This highlights an important tradeoff in building predictive models.
# By tuning the cp, we improved the accuracy by over 1%, but our tree become much more complicated.
# In some cases this will be OK, but on others it may be better to use a simpler model.