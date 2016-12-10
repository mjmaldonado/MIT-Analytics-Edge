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

#################################################
#
# Problem set 3 - Identify patterns in renewable energy
#
#################################################
library(dplyr)

# read in the data
energy = read.csv('energy.xls')

energy.df = tbl_df(energy)

# take a look at the data
str(energy)

#################################################
#
# Column guide
#
# GenTotal                  = Annual generation of energy using all types of energy sources (coal, nuclear, hydroelectric, solar, etc.) normalized by the state population at a given year.
# GenTotalRenewable         = Annual generation of energy using all renewable energy sources normalized by the state population at a given year.
# GenHydro                  = Annual generation of energy using each type of energy source as a percent of the total energy generation.
# GenSolar                  = Annual generation of energy using each type of energy source as a percent of the total energy generation.
# GenTotalRenewableBinary   = 1 if generation from solar or other renewable energy sources increased between a year n and a year n+1. 0 if it did not increase.
# GenSolarBinary            = 1 if generation from solar or other renewable energy sources increased between a year n and a year n+1. 0 if it did not increase.
# AllSourcesCO2             = Annual emissions per state in metric tons, normalized by the respective state population at a given year and caused by all energy generation sources.
# AllSourcesSO2             = Annual emissions per state in metric tons, normalized by the respective state population at a given year and caused by all energy generation sources.
# AllSourcesNOx             = Annual emissions per state in metric tons, normalized by the respective state population at a given year and caused by all energy generation sources.
# EPriceResidential         = Average electricity price per state, per sector (residential, industrial, commercial, etc.)
# EPriceCommercial          = Average electricity price per state, per sector (residential, industrial, commercial, etc.)
# EPriceIndustrial          = Average electricity price per state, per sector (residential, industrial, commercial, etc.)
# EPriceTransportation      = Average electricity price per state, per sector (residential, industrial, commercial, etc.)
# EPriceTotal               = Average electricity price per state, per sector (residential, industrial, commercial, etc.)
# ESalesResidential         = Annual normalized sales of electricity per state, per sector.
# ESalesCommercial          = Annual normalized sales of electricity per state, per sector.
# ESalesIndustrial          = Annual normalized sales of electricity per state, per sector.
# ESalesTransportation      = Annual normalized sales of electricity per state, per sector.
# ESalesTotal               = Annual normalized sales of electricity per state, per sector.
# CumlRegulatory            = Number of energy-related financial incentives and regulations created by a state per year.
# CumlFinancial             = Number of energy-related financial incentives and regulations created by a state per year.
# Total.salary              = Demographic data such as annual wages per capita and presidential results (0 if a state voted republican, 1 is democrat).
# presidential.results      = Demographic data such as annual wages per capita and presidential results (0 if a state voted republican, 1 is democrat).
# Import                    = Demographic data such as annual wages per capita and presidential results (0 if a state voted republican, 1 is democrat).
#
##################################################

# 3.1 - Which state in the US seems to have the highest local generation of energy from renewable sources?
energy[which.max(energy$GenTotalRenewable),]

# 3.2 - What is the average CO2 emissions from all sources of energy for:
# states during years in which they voted republican
# states during years in which they voted democrat
# state that voted deomcrate have on average higher NOx emissions than states that voted republican across all years; T or F?
tapply(energy$AllSourcesCO2, as.factor(energy$presidential.results), mean, na.rm = TRUE)

mean(dem$AllSourcesNOx, na.rm = TRUE) > mean(rep$AllSourcesNOx, na.rm = TRUE)

# 3.3 - What is the correlation between overall CO2 emissions and energy sales made to industrial facilities?
cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use = "complete")

# based on the three correlation plots below we can see that S02 emissions are likely higher with increased industrial energy sales
energy.df %>% select(EsalesIndustrial, AllSourcesSO2) %>% lattice::splom()
energy.df %>% select(EsalesResidential, AllSourcesNOx) %>% lattice::splom()
energy.df %>% select(EsalesCommercial, AllSourcesCO2) %>% lattice::splom()

# 3.4 - Create a boxplot of the total energy price by state across the data and a table summarizing the mean of EPriceTotal by State
boxplot( EPriceTotal ~ STATE, data = energy )

# which state has the lowest average energy price of all?
tapply(energy$EPriceTotal, energy$STATE, mean, na.rm = TRUE) %>% sort() %>% head()

# Is this state associated with the highest mean total energy generation?
tapply(energy$GenTotal, energy$STATE, mean, na.rm = TRUE) %>% sort() %>% tail()

# 3.5 - Split the data into a training and test set and build a logistic regression model to predict GenSolarBinary.
# Use: GenHydro, GenSolar, CumlFinancial, CumlRegulatory, Total.salary, Import

library(caTools)
set.seed(144)
split = sample(1:nrow(energy), size = 0.7 * nrow(energy))
train = energy[split,]
test  = energy[-split,]

energy.LR = glm( GenSolarBinary ~ 
                   GenHydro + 
                   GenSolar + 
                   CumlFinancial + 
                   CumlRegulatory + 
                   Total.salary + 
                   Import,
                 data = train,
                 family = "binomial"
                 )

# which variable is most predictive in the model?
summary(energy.LR)

# 3.6 - Compute the predictions on the test set.  Using a threshold of 0.5, what is the accuracy of our model on the test set?
energy.pred = predict( energy.LR, newdata = test, type = "response" )
conf.mat.LR = table( test$GenSolarBinary, energy.pred > 0.5 )
accuracy.LR = sum( diag( conf.mat.LR ) ) / nrow(test)

# What is the accuracy for states voting republican
rep = test %>% filter(presidential.results == 0)
rep.energy.pred = predict( energy.LR, newdata = rep, type = "response")
rep.conf.mat.LR = table(rep$GenSolarBinary, rep.energy.pred > 0.5)
rep.accuracy.LR = sum( diag( rep.conf.mat.LR ) ) / nrow(rep)

# What is the accuracy for states voting democrat?
dem = test %>% filter(presidential.results == 1)
dem.energy.pred = predict( energy.LR, newdata = dem, type = "response")
dem.conf.mat.LR = table(dem$GenSolarBinary, dem.energy.pred > 0.5)
dem.accuracy.LR = sum( diag( dem.conf.mat.LR ) ) / nrow(dem)

# 3.7 - Let's see if we can improve our accuracy with the cluster-then-predict (ctp) method
# We are interested in clustering observations together based on:
  # CumlRegulatory, CumlFinancial, presidential.results, Total.salary, and Import

# first create a new test and training set
library(caret)
library(flexclust)
train.ctp = train %>% select(CumlRegulatory, CumlFinancial, presidential.results, Total.salary, Import)
test.ctp  = test %>% select(CumlRegulatory, CumlFinancial, presidential.results, Total.salary, Import)

# normalize the data for clustering
preproc = preProcess(train.ctp)
train.norm = predict(preproc, train.ctp)
test.norm  = predict(preproc, test.ctp)

# use k-means clustering with k = 2 and the maximum number of iterations = 1000
set.seed(144)
k = 2
train.kmeans = kmeans(train.norm, centers = k, iter.max = 1000)

# using flexclust, perform a K-Mean Centroid Cluster Analysis
km.kcca = as.kcca(train.kmeans, train.norm)

# model the clusters to the training and test set
cluster.train = predict(km.kcca)
cluster.test  = predict(km.kcca, newdata = test.norm)

# apply the model back to the training set
train.cluster1 = subset(train, cluster.train == 1)
train.cluster2 = subset(train, cluster.train == 2)

# compare the two datasets
summary(train.cluster1); summary(train.cluster2)

# 3.8 - Using the variables GenHydro, GenSolar, CumlFinancial, CumlRegulatory, Total.salary, and Import build a logistic regression model on the first cluster
cluster1.LR = glm( GenSolarBinary ~ 
                     GenHydro + 
                     GenSolar + 
                     Total.salary +
                     CumlRegulatory + 
                     CumlFinancial +
                     Import,
                   data = train.cluster1,
                   family = "binomial"
                   )

# which variable is most predictive?
summary(cluster1.LR)

# 3.9 - what is the accuracy of test.cluster1?
test.cluster1 = subset(test, cluster.test == 1)
cluster1.pred = predict( cluster1.LR, newdata = test.cluster1, type = "response")
cluster1.conf.mat = table( test.cluster1$GenSolarBinary, cluster1.pred > 0.5 )
cluster1.accuracy = sum( diag( cluster1.conf.mat ) ) / nrow(test.cluster1)

# is this better than the original model?
mod.compare.accuracy = sum( diag( table( test.cluster1$GenSolarBinary, predict( energy.LR, newdata = test.cluster1, type = "response") > 0.5 ) ) ) / nrow( test.cluster1)

# 3.10 - Using the variables GenHydro, GenSolar, CumlFinancial, CumlRegulatory, Total.salary, and Import build a logistic regression model on the second cluster
cluster2.LR = glm( GenSolarBinary ~ 
                     GenHydro + 
                     GenSolar + 
                     Total.salary +
                     CumlRegulatory + 
                     CumlFinancial +
                     Import,
                   data = train.cluster2,
                   family = "binomial"
)

# how does this model compare to cluster1.LR?
summary(cluster1.LR); summary(cluster2.LR)

# 3.11 - Using the threshold of 0.5, what is the accuracy on test.cluster2?
test.cluster2 = subset(test, cluster.test == 2)
cluster2.pred = predict( cluster2.LR, newdata = test.cluster2, type = "response")
cluster2.conf.mat = table( test.cluster2$GenSolarBinary, cluster2.pred > 0.5 )
cluster2.accuracy = sum( diag( cluster2.conf.mat ) ) / nrow(test.cluster2)

# is this better than the original model?
mod2.compare.accuracy = sum( diag( table( test.cluster2$GenSolarBinary, predict( energy.LR, newdata = test.cluster2, type = "response") > 0.5 ) ) ) / nrow( test.cluster2)

# 3.12 - To compute the overall test-set accuracy of the cluster-then-predict approach, combine all the test-set predictions into a single vector "all.predictions" and all the true otucomes into a single vector "all.outcomes"
all.predictions = c(cluster1.pred, cluster2.pred)
all.outcomes = c(test.cluster1$GenSolarBinary, test.cluster2$GenSolarBinary)

# what is the overall accuracy on the test set, using the cluster-then-predict approach (using a threshold of 0.5)?
accuracy.overall = sum( diag( table(all.outcomes, all.predictions > 0.5) ) ) / sum( table(all.outcomes, all.predictions > 0.5) )

# so it looks like we are performing much better with a cluster-then-predict approach!
