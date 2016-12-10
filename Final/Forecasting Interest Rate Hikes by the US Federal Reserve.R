##############################################
#
# Author: Michael Maldonado
#
# Description: Attempt to predict when the Federal Reserve will raise interest rates
#
##############################################

# clear some space from previous work
rm( list = ls() )

# read in the new data
fed = read.csv('federalFundsRate.csv', stringsAsFactors = FALSE)

# take a look at our data
str(fed)

#############################################
#
# Column guide
#
# Date                = The date the change was announced
# Chairman            = The name of the Federal Reserve Chairman at the time the change was announced
# PreviousRate        = The federal funds rate in the prior month
# Streak              = The current streak of raising or not raising the rate, e.g. +8 indicates the rate has been increased 8 months in a row, whereas -3 indicates the rate has been lowered or stayed the same for 3 months in a row.
# GDP                 = The U.S. Gross Domestic Product, in Billions of Chained 2009 US Dollars
# Unemployment        = The unemplyment rate in the U.S.
# CPI                 = The Consumer Price Index, an indicator of inflation, in the U.S.
# HomeownershipRate   = The rate of homeownership in the U.S.
# DebtAsPctGDP        = The U.S. national debt as a percentage of GDP
# DemocraticPres      = Whether the sitting U.S. President is Democrate (1) or Republican (0)
# MonthsUntilElection = The number of remaining months until the next U.S. presidential election.
# RaisedFedFunds      = 1 if the funds rate increased, 0 otherwise
#
#############################################

# 1.1 - What proportion of months did the Fed raise the interest rate?
mean(fed$RaisedFedFunds)

# 1.2 - Which Federal Reserve Chair presided over the most interest rate decisions?
sort(table(fed$Chairman))

# 1.3 - Convert the following variable to factors:

  # Chairman; DemocraticPres; RaisedFedFunds;

fed$Chairman = as.factor(fed$Chairman); fed$DemocraticPres = as.factor(fed$DemocraticPres); fed$RaisedFedFunds = as.factor(fed$RaisedFedFunds)

# which classification model requires the dependent variable to be stored as a factor?
# Random Forest

# 1.4 create a training and test set
library(caTools)
set.seed(201)
split = sample.split(fed$RaisedFedFunds, 0.7)
train = subset(fed, split == TRUE)
test  = subset(fed, split == FALSE)

# Why do we use the sample.split() function to split the data into a training and test set?
# It balances the dependent variable between the training and testing sets
# This information can be found via ?sample.split()

# 1.5 - train a logisitic regression model
LR = glm( RaisedFedFunds ~ 
            PreviousRate +
            Streak +
            Unemployment +
            HomeownershipRate +
            DemocraticPres +
            MonthsUntilElection,
          family = "binomial",
          data = train
          )

# Which variables appear to be the most statistically significant with an increased chance of the federal funds rate being raised?
summary(LR)

# 1.6 - What is the predicted probability that the interest rate will be raised with the given variables:
  # PreviousRate = 0.017  
  # Streak = -3
  # Unemployment = 0.051
  # HomeownershipRate = .653
  # DemocraticPres = 0
  # MonthsUntilElection = 18

# The equation for the predicted probability of a logistic regression is:
  # 1 / (1 + exp(-(sum(coefs))))

prob.fnc = as.vector(LR$coefficients[-1]) * c(1.7, -3, 5.1, 65.3, 0, 18)
pred.prob.1.6 = 1 / (1 + exp(-(LR$coefficients[[1]] + sum(prob.fnc))))

# 1.7 - What is the meaning of the coefficient labeled "DemocraticPres1" in the logistic regression summary output?
# When the president is Democratic, the odds of the federal funds rate increasing is 41.6% higher than in an otherwise identical month

# 1.8 - What is the number of test set observations where the prediction from the logistic regression model is different than the prediction from the baseline model?
LR.baseline = max( table( train$RaisedFedFunds ) ) / nrow(train)

LR.pred = predict( LR, newdata = test, type = "response" )
conf.mat.LR = table( test$RaisedFedFunds, LR.pred > 0.5 )
conf.mat.LR

# The baseline model would always predict 1 for RaisedFedFunds, so we want to know the number of observations which are predicted 0 in our model
# in this case, it's the first column of the confusion matrix
sum(conf.mat.LR[,1])

# 1.9 - What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpred = prediction(LR.pred, test$RaisedFedFunds)
LR.auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# 1.10 - What is the meaning of the AUC?
# The proportion of the time the model can differentiate between a randomly selected month during which the federal funds were raised
# and a randomly selected month during which the federal funds were not raised

# 1.11 - Which logistic regression threshold is associated with the upper right corner of the ROC plot?
ROCRperf = performance( ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, main = "Receiver Operator Characteristic Curve", print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

# 1.12 - At roughly which logistic regression cutoff does the model achieve a true positive rate of 85% and a false positive rate of 60%?
# Look at the plot created by the above statement

# 1.13 - Which best describes how 10-fold cross-validation works when selectiong between 2 different parameter values?
# 20 models are trained on subsets of the training set and evaluated on a portion of the training set

# 1.14 - Cross-validate a CART model and determine the cp value which maximizes the cross-validation accuracy
library(caret)

num.folds = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.001, 0.05,0.001))

set.seed(201)
CART.cv = train(
  RaisedFedFunds ~ 
    PreviousRate + 
    Streak + 
    Unemployment + 
    HomeownershipRate + 
    DemocraticPres + 
    MonthsUntilElection, 
  data = train, 
  method = "rpart", 
  trControl = num.folds,
  tuneGrid = cp.grid
)

CART = rpart( 
  RaisedFedFunds ~ 
    PreviousRate + 
    Streak + 
    Unemployment + 
    HomeownershipRate + 
    DemocraticPres + 
    MonthsUntilElection,
  data = train,
  method = "class",
  cp = CART.cv$bestTune[[1]]
)

# 1.15 - What variable is used as the first (upper-most) split in the tree?
library(rpart.plot)
prp(CART)

# 1.16 - What would your prediction be from the same parameters as 1.6 using the CART model?
# Use the plot from above

# 1.17 - What is the accuracy of the CART model?
CART.pred = predict( CART, newdata = test, type = "class" )
CART.acc = sum( diag( table( test$RaisedFedFunds, CART.pred ) ) ) / nrow(test)

##########################################
#
# Problem set 2 - Market Segmentation
#
##########################################

# We will be using the Households dataset
h = read.csv('Households_Final.csv')

# check out the structure of the data
str(h)

##########################################
#
# Column guide
#
# NumVisits     = the number of times the household visited the retailer
# AvgProdCount  = the average number of products purchased per transaction
# AvgDiscount   = the average discount per transaction from coupon usage (in %) - NOTE: Do not divide this value by 100!
# AvgSalesValue = the average sales value per transaction
# MorningPct    = the percentage of visits in the morning (8am - 1:59 pm)
# AfternoonPct  = the percentage of visits in the afternoon (2pm - 7:59 pm)
#
# note: some visits can occur outside of morning and afternoon houss.  That is, visit from 8pm - 7:59 am.
#
##########################################

# 2.1.a - How many households have logged transactions at the retailer only in the morning?
table(h$MorningPct == 100)

# 2.1.b - How many households have logged transactions at the retailer only in the afternoon?
table(h$AfternoonPct == 100)

# 2.2.a - Of the households that spend more than $150 per transaction on average, what is the minimum average discount per transaction?
min( subset( h, h$AvgSalesValue > 150 )$AvgDiscount )

# 2.2.b - Of the households that have an average discount per transaction greater than 25%, what is the minimum average sales value per transaction?
min( subset( h, h$AvgDiscount > 25 )$AvgSalesValue )

# 2.2.c - What proportion of housholds visited the retailer at least 300 times?
table(h$NumVisits >= 300)[2] / nrow(h)

# 2.3 - When clustering data, it is often important to normalize the vaiables so they are all on the same scale.
# If we clustered this dataset without normalizing, which variable would we expect to dominate in the distance calculations?
# Think about the variable that has much higher values than the rest

# 2.4 - normalize the households dataset
preproc = preProcess(h)
h.norm = predict(preproc, h)

# what is the maximum value of NumVisits in the normalized dataset?
# what is the minimum value of the AfternoonPct in the normalized datset?
summary(h.norm)

# 2.5 - create a dendrogram of the normalized dataset
set.seed(200)
distances = dist(h.norm, method = "euclidean")
cluster.shoppers = hclust(distances, method = "ward.D")
plot(cluster.shoppers, labels = FALSE)

# How many clusters seem appropriate for our problem?
# Think about the distance between each break in the dendrogram

# 2.6 - Use k-means clustering on the normalized dataset, with 10 clusters

set.seed(200)
k = 10
h.kmeans = kmeans(h.norm, centers = k)

# how many observations are in the smallest cluster?
# how many observations are in the largest cluster?
str(h.kmeans)

# 2.7; 2.8; 2.9;
# Which cluster best fits the description "morning shoppers stopping in to make a quick purchase"?
# Which cluster best fits the description "shoppers with high average product count and high average value per visit"?
# Which cluster best fits the description "frequent shoppers with lower value per visit?
library(dplyr)

h %>% subset(h.kmeans$cluster == 1) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 2) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 3) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 4) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 5) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 6) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 7) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 8) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 9) %>% colMeans() 
h %>% subset(h.kmeans$cluster == 10) %>% colMeans() 

# 2.10.a - If we ran hierarchical clustering a second time without making any addition calls to set.seet, we would expect:
# Identical results

# 2.10.b - If we ran k-means clustering a second time without making additional calls to set.seed, we would expect:
# Different results

# 2.10.c - If we ran k-means clustering a second time, again running the command set.seet(200) right before doing the clustering, we would expect:
# Identical results

# 2.10.d - If we ran k-means clustering a second time, running the command set.seed(100) right before doing the clustering, we would expect:
# Different results

# 2.11 - Suppose the marketing department at the retail store decided that the 10 clusters were too specific, and they wanted more general clusters to describe the consumer base.
# Would they want to increase or decrease the number of clusters?
# Decrease

# 2.12 - Run the k-means clustering again, but this time with 5 clusters
set.seed(5000)
h.kmeans.5.clusters = kmeans(h.norm, centers = 5)

# how many observations are in the smallest cluster?
# how many observations are in the largest cluster?
str(h.kmeans.5.clusters)

# 2.13 - Using the new cluster assignments, which cluster best fits the description "frequent shoppers with low value per visit"?
h %>% subset(h.kmeans.5.clusters$cluster == 1) %>% colMeans()
h %>% subset(h.kmeans.5.clusters$cluster == 2) %>% colMeans()
h %>% subset(h.kmeans.5.clusters$cluster == 3) %>% colMeans()
h %>% subset(h.kmeans.5.clusters$cluster == 4) %>% colMeans()
h %>% subset(h.kmeans.5.clusters$cluster == 5) %>% colMeans()

# 2.14 - Why do we use cluster centroids to describe the clusters?
# The cluster centroid captures the average behavior in the cluster, and can be used to summarize the general pattern in the cluster.

# 2.15 - What would be a good visulization to show the distribution of the clusters?
# A box plot or histogram
