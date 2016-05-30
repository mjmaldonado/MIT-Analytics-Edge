###########################################
#
# Week 6 Homework - clustering
#
# Author: Michael Maldonado
#
###########################################

###########################################
#
# Problem Set 1 - Analysis of articles and opinions from the Daily Kos
#
###########################################

# clear some space
rm( list = ls() )

# read in the data
kos = read.csv('dailykos.csv')

# take a look at the structure of the data
str(kos)

# use hierarchical clustering to build our model
# first we need to compute the eucildean distance
distance = dist(kos, method = "euclidean")

# now cluster the data
kos.cluster = hclust(distance, method = "ward.D")

# 1.1.1 - running the dist function will probably take you a while.  Why?
# There are many observations as well as many variables so the distance computation will take a while

# 1.1.2 - Using a dendrogram, how many clusters look like a good choice for our model?
plot(kos.cluster)

# 1.1.3 - In our particular application, we are trying to cluster news articles and blog posts into groups.
# This can be used to show readers categories to choose from when trying to decide what to read.
# Just thinking about this application, what are good choices for the number of clusters?
rect.hclust(kos.cluster, k = 2, border = "blue")
rect.hclust(kos.cluster, k = 3, border = "green")
rect.hclust(kos.cluster, k = 7, border = "red")
rect.hclust(kos.cluster, k = 8, border = "orange")

# 1.1.4 - Let's pick 7 clusters; this number is reasonable according to the dendrogram, and also seems reasonable for the application.
# use cutree to split the data into 7 clusters
kos.groups = cutree(kos.cluster, k = 7)

# we don't really want to use tapply here because we have 1,000+ different variables.
# instead let's split our data into 7 datasets
grp1 = subset(kos, kos.groups == 1)
grp2 = subset(kos, kos.groups == 2)
grp3 = subset(kos, kos.groups == 3)
grp4 = subset(kos, kos.groups == 4)
grp5 = subset(kos, kos.groups == 5)
grp6 = subset(kos, kos.groups == 6)
grp7 = subset(kos, kos.groups == 7)

# how many observations are in cluster three?
# If you're using RStudio, take a look in your environment

# 1.1.5 - What is the most frequent word in cluster 1, in terms of average value?
library(dplyr)

grp1 %>% colMeans() %>% sort() %>% tail()
# take the grp1 data
# then compute the mean frequence of each of the words in grp1
# then sort the words based on the value computed above
# then only show the top 6 words

# 1.1.6 - Now repeat this for each cluster to answer the following questions
grp2 %>% colMeans() %>% sort() %>% tail()
grp3 %>% colMeans() %>% sort() %>% tail()
grp4 %>% colMeans() %>% sort() %>% tail()
grp5 %>% colMeans() %>% sort() %>% tail() # iraq war
grp6 %>% colMeans() %>% sort() %>% tail()
grp7 %>% colMeans() %>% sort() %>% tail() # democratic cluster

# 1.2.1 - Now use k-means clustering to perform the clustering
k = 7
set.seed(1000)
kos.kmeans = kmeans(kos, centers = k)

# how many obersvarvations are in cluster 3?
# which cluster has the most observations?
# which cluster has the fewest observations?
str(kos.kmeans)

# 1.2.2
# Which k-means cluster best corresponds to the Iraq War?
# Which k-means cluster best corresponds to the Democratic Party?
kos %>% subset(kos.kmeans$cluster == 1) %>% colMeans() %>% sort() %>% tail()
kos %>% subset(kos.kmeans$cluster == 2) %>% colMeans() %>% sort() %>% tail() # democratic party
kos %>% subset(kos.kmeans$cluster == 3) %>% colMeans() %>% sort() %>% tail() # iraq war
kos %>% subset(kos.kmeans$cluster == 4) %>% colMeans() %>% sort() %>% tail()
kos %>% subset(kos.kmeans$cluster == 5) %>% colMeans() %>% sort() %>% tail()
kos %>% subset(kos.kmeans$cluster == 6) %>% colMeans() %>% sort() %>% tail()
kos %>% subset(kos.kmeans$cluster == 7) %>% colMeans() %>% sort() %>% tail()

# 1.2.3 - Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
table(kos.kmeans$cluster == 2, kos.groups)

# 1.2.4 - Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
table(kos.kmeans$cluster == 3, kos.groups)

# 1.2.5 - How about for K-Means Cluster 7?
table(kos.kmeans$cluster == 7, kos.groups)

# 1.2.5 - How about for K-Means Cluster 6?
table(kos.kmeans$cluster == 6, kos.groups)

###########################################
#
# Problem Set 2 - Market segmentation for airlines
#
###########################################

# In this problem set, we will use clustering to find
# similar groupos of customers who belong to an airline's frequent flyer program.
# The airline is tryin to learn more about its customers so that it can target different
# customer segments with different types of mileage offers

# We will use a datset called AirlineCluster.csv which contains information on 3,999
# members of the frequent flyer program.

##########################################
#
# Column guide
#
# Balance         = number of miles eligible for award travel
# QualMiles       = number of miles qualifying for TopFlight status
# BonusMiles      = number of miles earned from non-flight bonus transactions in the past 12 months
# BonusTrans      = number of non-flight bonus transactions in the past 12 months
# FlightMiles     = number of flight miles in the past 12 months
# FlightTrans     = number of flight transactions in the past 12 months
# DaysSinceEnroll = number of days since enrolled in the frequent flyer program
#
##########################################

# clear some space
rm( list = ls() )

# read in the data
air = read.csv('AirlinesCluster.csv')

# 2.1.1
# Which TWO variables have, on average, the smallest values?
# Which TWO variables have, on average, the largest values?
summary(air)

# 2.1.2 - Why is it important to normalize data before clustering?
# If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale.

# 2.1.3 - Normalize the data.
# Which variable has the largest maximum value?
# Which variable has the smallest minimum value?
library(caret)

air.preproc = preProcess(air) # pre-process the data
air.norm    = predict(air.preproc, air) # perform the normailization

summary(air.norm)

# 2.2.1 - Use Hierarchical Clustering and then plot 0the dendrogram

# compute the euclidean distance
distance = dist(air.norm, method = "euclidean")

# perform the Hierarchical Clustering
air.hcluster = hclust(distance, method = "ward.D")

# plot the dendrogram
plot(air.hcluster)

# suppose the airline is looking for somewhere between 2 and 10 clusters.
# according to the dendrogram, which of the following is NOT a good choice for the number of clusters?
rect.hclust(air.hcluster, k = 6, border = "green")

# 2.2.2 - Suppose after looking at the dendrogram and discussing with the marketing department
# the airline decides to proceed with 5 clusters.  Perform this division.
# How many data points are in Cluster 1?
air.clusters = cutree(air.hcluster, k = 5)

table(air.clusters)

# 2.2.3 - 2.2.7
# compared to other clusters, which variables have the largest values (if any) in each cluster?
# how would you describe the customers in said cluster?
for (name in names(air)) {
  print(name)
  print(tapply(air[[name]], air.clusters, mean))
}

# you could also use:
# colMeans(subset(air, air.cluster == INSERT_CLUSTER_HERE))

# lapply(split(air, air.clusters), colMeans)

# split divides data in the vector x into the groups defined by f.
# The replacement forms replace values corresponding to such a division.  unplit reverses the effect of split
# split(x, f, drop= FALSE, ...)

# 2.3.1 - Now use K-Means Clustering on the normalized data.
k = 5
set.seed(88)

air.kmeans = kmeans(air.norm, centers = k, iter.max = 1000)

# how many clusters have more than 1000 observations?
str(air.kmeans)

# compare the cluster centroids from the K-Means and Hierarchical Clustering
# do you expect Cluster 1 of the K-Means clustering output to be necessarily similar to Cluster 1 of the Hierarchical clustering output?
table(air.clusters, air.kmeans$cluster)


###############################################
#
# Problem Set 3 - Predicting stock returns with cluster-then-predict
#
###############################################

# When selecting stocks to invest in, investors seek to obtain good future returns.
# In this problem, we will first use clustering to identify clusters of stocks that have similar returns over time.
# Then, we'll use logistic regression to predict whether or not the stocks will have positive future returns

# We will use StocksCluster.csv which contains monthly stock returns from the NASDAQ stock exchange.
# The data comes from infochimps

# Each observation in the dataset is the monthly returns of a particular company in a particular year
# The years are 2000 - 20009.  The companies are limited to tickers that were listed on the exhange for the entire period, and whose stock price never fell below $1.

# For the first 11 variables, the value stored is a proportional change in the stock value during that month.
# For instance, a value of 0.05 means the stock increased in value 5% during the month.

###############################################
#
# Column guide
#
# ReturnJan   = the return for the company's stock during January (in the year of the observation). 
# ReturnFeb   = the return for the company's stock during February (in the year of the observation). 
# ReturnMar   = the return for the company's stock during March (in the year of the observation). 
# ReturnApr   = the return for the company's stock during April (in the year of the observation). 
# ReturnMay   = the return for the company's stock during May (in the year of the observation). 
# ReturnJune  = the return for the company's stock during June (in the year of the observation). 
# ReturnJuly  = the return for the company's stock during July (in the year of the observation). 
# ReturnAug   = the return for the company's stock during August (in the year of the observation). 
# ReturnSep   = the return for the company's stock during September (in the year of the observation). 
# ReturnOct   = the return for the company's stock during October (in the year of the observation). 
# ReturnNov   = the return for the company's stock during November (in the year of the observation). 
# PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation). This variable takes value 1 if the return was positive, and value 0 if the return was not positive.
#
###############################################

# clear some space
rm( list = ls() )

# read in the data
stocks = read.csv('StocksCluster.csv')

# 3.1.1 - How many observations are in the dataset?
# Use RStudio's enviorment

# 3.1.2 - What proportion of observations have positive returns in December?
table(stocks$PositiveDec)[2] / nrow(stocks)

# or mean(stocks$PositiveDec)

# 3.1.3 - What is the maximum correlation between any two return variables in the dataset?
max(cor(stocks)[cor(stocks) != 1])

# 3.1.4
# Which month (from Jan to Nov) has the largest mean across all observations?
# Which month (from Jan to Nov) has the smallest mean across all observations?
summary(stocks)

# 3.2.1 - Build a logistic regression model on the stocks data, attempting to predict PositiveDec
# build a training and test set
library(caTools)
set.seed(144)
split = sample.split(stocks$PositiveDec, SplitRatio = 0.70)
train = subset(stocks, split == TRUE)
test  = subset(stocks, split == FALSE)

# create the logistic regression
stocks.LR = glm(PositiveDec ~ ., family = "binomial", data = train)

# make a prediction on the training set
stocks.prediction = predict(stocks.LR, type = "response")

# build a confusion matrix using a threshold of 0.5
conf.mat.train.LR = table(train$PositiveDec, stocks.prediction > 0.5)

# calculate the accuracy
accuracy.LR = sum( diag( conf.mat.train.LR ) ) / nrow(train)

# 3.2.2 - Now obtain test set predictions.
# What is the overall accuracy of the model on the test set using a 0.5 threshold
stocks.test.prediction = predict(stocks.LR, newdata = test, type = "response")
conf.mat.test.LR = table(test$PositiveDec, stocks.test.prediction > 0.5)
accuracy.LR = sum( diag( conf.mat.test.LR ) ) / nrow(test)

# 3.2.3 - What is the accuracy on the test set of a baseline model?
accuracy.bl = mean(test$PositiveDec)

# 3.3.1 - Now let's cluster the stocks
# first we need to remove the dependent variable from the data
lim.train = train; lim.train$PositiveDec = NULL
lim.test  = test;  lim.test$PositiveDec = NULL

# why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
# Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology

# 3.3.2 - preprocess the data
library(caret)
preproc = preProcess(lim.train)
norm.train = predict(preproc, lim.train)
norm.test  = predict(preproc, lim.test)

# What is the mean of ReturnJan in the normalized training set?
mean(norm.train$ReturnJan)

# What is the mean of ReturnJan in the normalized test set?
mean(norm.test$ReturnJan)

# 3.3.3 - Why is the mean ReturnJan variable much closer to 0 in norm.train than in norm.test?
# The distribution on the ReturnJan variable is different in the training and testing set

# 3.3.4 - Now perform k-means clustering using 3 clusters on norm.train
set.seed(144)
k = 3
stocks.kmeans = kmeans(norm.train, centers = k)

# which cluster has the largest number of observations?
str(stocks.kmeans)

# 3.3.5 - Use flexclust to obtain training and test set cluster assignments
library(flexclust)

# assign the K-Mean Centroid Cluster Analysis
km.kcca = as.kcca(stocks.kmeans, norm.train)

# make a prediction of clusters on the training set
cluster.train = predict(km.kcca)

# do the same for the test set
cluster.test = predict(km.kcca, newdata = norm.test)

# How many test set observations were assigned to cluster 2?
table(cluster.test)

# 3.4.1
# Build data frames for each cluster on the training and test set
stocks.train1 = subset(train, cluster.train == 1)
stocks.train2 = subset(train, cluster.train == 2)
stocks.train3 = subset(train, cluster.train == 3)

stocks.test1 = subset(test, cluster.test == 1)
stocks.test2 = subset(test, cluster.test == 2)
stocks.test3 = subset(test, cluster.test == 3)

# Which training set data frame has the highest average value of the dependent variable?
tapply(train$PositiveDec, cluster.train, mean)

# 3.4.2 - build a logistic regression model for each of the clustered training sets
stock.LR1 = glm(PositiveDec ~. , family = "binomial", data = stocks.train1)
stock.LR2 = glm(PositiveDec ~. , family = "binomial", data = stocks.train2)
stock.LR3 = glm(PositiveDec ~. , family = "binomial", data = stocks.train3)

# Which variables have a positive sign for the coefficient in at least one of the different models AND a negative sign in one of the other models?
stock.LR1$coefficients > 0
stock.LR2$coefficients > 0
stock.LR3$coefficients > 0

# 3.4.3 - Now make predictions on the test sets.  What is the accuracy of each model using a threshold of 0.5
predict.LR1 = predict(stock.LR1, newdata = stocks.test1, type = "response")
predict.LR2 = predict(stock.LR2, newdata = stocks.test2, type = "response")
predict.LR3 = predict(stock.LR3, newdata = stocks.test3, type = "response")

conf.mat.LR1 = table(stocks.test1$PositiveDec, predict.LR1 > 0.5)
conf.mat.LR2 = table(stocks.test2$PositiveDec, predict.LR2 > 0.5)
conf.mat.LR3 = table(stocks.test3$PositiveDec, predict.LR3 > 0.5)

accuracy.LR1 = sum( diag( conf.mat.LR1 ) ) / nrow(stocks.test1)
accuracy.LR2 = sum( diag( conf.mat.LR2 ) ) / nrow(stocks.test2)
accuracy.LR3 = sum( diag( conf.mat.LR3 ) ) / nrow(stocks.test3)

# 3.4.4 - Let's combine all of the test set predictions into a single vector so that we can determine the overall performance of the cluster-then-predict method
all.predictions = c(predict.LR1, predict.LR2, predict.LR3)

all.outcomes = c(stocks.test1$PositiveDec, stocks.test2$PositiveDec, stocks.test3$PositiveDec)

# what is the overall test-set accuracy of the cluster-then-predict approach?
accuracy.overall = sum( diag( table(all.outcomes, all.predictions > 0.5) ) ) / sum( table(all.outcomes, all.predictions > 0.5) )

# We see a modest improvemtn over the original logistic regression model.
# Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy.
# By investing in stocks for which we are more confident that they will have positive returns (by selecting the ones with higher predicted probabilities),
# this cluster-then-predict model can give us an edge over the original logistic regression model.