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

