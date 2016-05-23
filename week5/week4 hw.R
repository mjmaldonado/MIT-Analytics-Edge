######################################
#
# week 4 hw
#
# Author: Michael Maldonado
#
######################################

######################################
#
# Problem Set 1 - Wikipedia revision history
#
######################################

######################################
#
# Column guide
#
# Vandal    = 1 if this edit was vandalism, 0 if not.
# Minor     = 1 if the user marked this edit as a "minor edit", 0 if not.
# Loggedin  = 1 if the user made this edit while using a Wikipedia account, 0 if they did not.
# Added     = The unique words added.
# Removed   = The unique words removed.
#
######################################

# clear some space
rm( list = ls() )

# read in the data
wiki = read.csv('wiki.csv', stringsAsFactors = FALSE)

# convert the Vandal variable to a factor
wiki$Vandal = as.factor(wiki$Vandal)

# 1.1.1 - how many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

# 1.1.2
# Now it's time to pre-process the data.  The current dataset is already stripped of punctation and is all lowercase.
library(tm)

# create the corpus
corpus = Corpus(VectorSource(wiki$Added))

# remove stop words
corpus = tm_map(corpus, removeWords, stopwords("english"))

# stem the words
corpus = tm_map(corpus, stemDocument)

# create a document term matrix
dtm.Added = DocumentTermMatrix(corpus)

# how many words are in the matrix?
dtm.Added

# 1.1.3 - filter out sparse terms by keepin only terms that appear in 0.3% ore more of the revisions.
sparse.Added = removeSparseTerms(dtm.Added, 0.997)

# how many terms are in the pruned matrix?
sparse.Added

# 1.1.4
# convert the matrix to a data frame
words.Added = as.data.frame(as.matrix(sparse.Added))

# prepend all of the words with the letter "A"
colnames(words.Added) = paste("A",colnames(words.Added))

# Now repeat all steps for creating an Added corpus for a Removed corpus (I will not be commenting this section)
corpus = Corpus(VectorSource(wiki$Removed))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm.Removed = DocumentTermMatrix(corpus)
sparse.Removed = removeSparseTerms(dtm.Removed, 0.997)
words.Removed = as.data.frame(as.matrix(sparse.Removed))
colnames(words.Removed) = paste("A", colnames(words.Removed))

# how many words are in the words.Removed data frame?
str(words.Removed)

# 1.1.5 - combine the two data frames
wiki.words = cbind(words.Added, words.Removed)

# add the Vandal column back to the data frame
wiki.words$Vandal = wiki$Vandal

# set up a training and test set
library(caTools)
set.seed(123)
split = sample.split(wiki.words$Vandal, SplitRatio = 0.70)
train = subset(wiki.words, split == TRUE)
test = subset(wiki.words, split == FALSE)

# what is the baseline accuracy using a modal prediction
conf.mat.bl = table(test$Vandal)
accuracy.bl = max(conf.mat.bl) / nrow(test)

# 1.1.6 - build a CART model on the training set
library(rpart)
library(rpart.plot)

# build the model
wiki.CART = rpart( Vandal ~ ., method = "class", data = train )

# make a prediction on the test set
predict.CART = predict( wiki.CART, newdata = test, type = "class" )

# what is the accuracy of this model?
conf.mat.CART = table( test$Vandal, predict.CART )
accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow(test)

# 1.1.7 - plot the CART.  How many words stems doe the CART model use?
prp(wiki.CART)

# 1.1.8 - Given the performance of the CART model relative to the baseline, what is the best explanation of these results?
# Althought this model out performs the baseline, bag of words is not very predictive for this problem


# 1.2.1
# We weren't able to improve on the baseline using the raw textual information. More specifically, the words themselves were not useful.
# There are other options though, and in this section we will try two techniques - identifying a key class of words, and counting words.

# The key class of words we will use are website addresses. "Website addresses" (also known as URLs - Uniform Resource Locators)
# are comprised of two main parts. An example would be "http://www.google.com". The first part is the protocol, which is usually
# "http" (HyperText Transfer Protocol). The second part is the address of the site, e.g. "www.google.com". We have stripped all
# punctuation so links to websites appear in the data as one word, e.g. "httpwwwgooglecom". We hypothesize that given that a lot
# of vandalism seems to be adding links to promotional or irrelevant websites, the presence of a web address is a sign of vandalism.

# make a new column to determine whether or not "http" was in Added
wiki.words.http = wiki.words
wiki.words.http$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE),1,0)

# based on this new column, how many revisions added a link?
table(wiki.words.http$HTTP)

# 1.2.2
# make two more training and test sets
train.http = subset(wiki.words.http, split == TRUE)
test.http = subset(wiki.words.http, split == FALSE)

# create a new CART model
wiki.http.CART = rpart(Vandal ~ ., method = "class", data = train.http)

# make a preiction on the test set
predict.http.CART = predict( wiki.http.CART, newdata = test.http, type = "class" )

# build a confusion matrix
conf.mat.http.CART = table( test$Vandal, predict.http.CART )

# what is the accuracy of this new model?
accuracy.http.CART = sum( diag( conf.mat.http.CART ) ) / nrow(test.http)

# 1.2.3
# Another possibility is that the number of words added and removed is predictive,
# perhaps more so than the actual words themselves. We already have a word count available
# in the form of the document-term matrices (DTMs).

wiki.words.http$NumWordsAdded = rowSums(as.matrix(dtm.Added))
wiki.words.http$NumWordsRemoved = rowSums(as.matrix(dtm.Removed))

# what is the average number of words added?
summary(wiki.words.http$NumWordsAdded)

# make a third set of training and test sets
train.added = subset(wiki.words.http, split == TRUE)
test.added = subset(wiki.words.http, split == FALSE)

# build a CART model on the new training set
wiki.added.CART = rpart( Vandal ~ ., method = "class", data = train.added )

# make a prediction on the next test set
predict.added.CART = predict( wiki.added.CART, newdata = test.added, type = "class" )

# build a confusion matrix
conf.mat.added.CART = table( test$Vandal, predict.added.CART )

# what is the accuracy of this model?
accuracy.added.CART = sum( diag( conf.mat.added.CART ) ) / nrow(test.added)

# 1.3.1
# We have two pieces of metadata, Minor and Loggedin
# Let's see how a CART model performs with these two variables

# first make a copy of the most recent data frame
wiki.words.meta = wiki.words.http

# now add the columns to the new data frame
wiki.words.meta$Minor = wiki$Minor
wiki.words.meta$Loggedin = wiki$Loggedin

# now split the data, build a CART model with the two added variables, make a prediction, and compute the accuracy
train.meta = subset(wiki.words.meta, split == TRUE)
test.meta = subset(wiki.words.meta, split == FALSE)
wiki.meta.CART = rpart( Vandal ~ ., method = "class", data = train.meta )
predict.meta.CART = predict( wiki.meta.CART, newdata = test.meta, type = "class" )
conf.mat.meta = table( test.meta$Vandal, predict.meta.CART )
accuracy.meta = sum( diag( conf.mat.meta ) ) / nrow(test.meta)

# 1.3.2 - there is a substantial difference in the accuracy of the model using the meta data.

# Is this because our model is more complicated?  Plot the CART tree and determine how many splits are in the tree.
prp(wiki.meta.CART)

# No, which is great becasue we were able to improve our model accuracy without making the model more complicated!

##########################################
#
# Problem set 2 - Automating reviews in Medicine
#
##########################################

##########################################
#
# Column guide
#
# title     = title of the publication
# abstract  = abstract of the publication
# trial     = indicator of whether or the paper is a clinical trial testing a drug therapy for cancer.
#
##########################################

# clear some space
rm( list = ls() )

# read in the data
med = read.csv('clinical_trial.csv', stringsAsFactors = FALSE)

# 2.1.1 - how many characters are there in the longest abstract?
max(nchar(med$abstract))

# 2.1.2 - how many search results provided no abstract?
nrow(subset(med, nchar(med$abstract) == 0))

# 2.1.3 - what is the text of the title which has the minimum number of characters in the title?
med$title[which.min(nchar(med$title))]

# 2.2.1 - Because we have both title and abstract information for trials, we need to build two corpera.
# pre-process the data and build corpera for both the title and abstract variables

library(tm)

# title corpus
corpus.title = Corpus(VectorSource(med$title))
corpus.title = tm_map(corpus.title, tolower)
corpus.title = tm_map(corpus.title, PlainTextDocument)
corpus.title = tm_map(corpus.title, removePunctuation)
corpus.title = tm_map(corpus.title, removeWords, stopwords("english"))
corpus.title = tm_map(corpus.title, stemDocument)

# build a data frame for analysis
dtm.title    = DocumentTermMatrix(corpus.title)
sparse.title = removeSparseTerms(dtm.title, 0.95)
title.df     = as.data.frame(as.matrix(sparse.title))

# abstract corpus
corpus.abstract = Corpus(VectorSource(med$abstract))
corpus.abstract = tm_map(corpus.abstract, tolower)
corpus.abstract = tm_map(corpus.abstract, PlainTextDocument)
corpus.abstract = tm_map(corpus.abstract, removePunctuation)
corpus.abstract = tm_map(corpus.abstract, removeWords, stopwords("english"))
corpus.abstract = tm_map(corpus.abstract, stemDocument)

# build a data frame for analysis
dtm.abstract    = DocumentTermMatrix(corpus.abstract)
sparse.abstract = removeSparseTerms(dtm.abstract, 0.95)
abstract.df     = as.data.frame(as.matrix(sparse.abstract))

# how many terms remain in each document?
str(title.df); str(abstract.df)

# 2.2.1 - What is the most likely reason why dtm.abstract has so many more terms than dtm.title?
# Abstracts tend to have many more words than titles

# 2.2.2 - what is the most frequent word stem across all the abstracts?
sort(colSums(abstract.df))

# 2.3.1 - combine the title and abstract data frames so we can make predictions
# first we need to differentiate the two
colnames(abstract.df) = paste0("A", colnames(abstract.df))
colnames(title.df)    = paste0("T", colnames(title.df))

trials = cbind(title.df, abstract.df)

# Add the trial variable back to the trials data frame
trials$trial = med$trial

# 2.3.3 - now build a test and training set
library(caTools)
set.seed(144)
split = sample.split(trials$trial, SplitRatio = 0.70)
train = subset(trials, split == TRUE)
test = subset(trials, split == FALSE)

# what is the accuracy of the baseline model?
conf.mat.bl = table(test$trial)
accuracy.bl = max(conf.mat.bl) / nrow(test)

# 2.3.4 - build a CART model using all independent variables
library(rpart)
library(rpart.plot)

trials.CART = rpart( trial ~ ., method = "class", data = train )

# plot the tree
prp(trials.CART)

# 2.3.5 - Obtain the training set prediction for the model
predicted.prob = predict(trials.CART)[,2]

# what is the maximum predicted probability
max(predicted.prob)

# 2.3.6 - without running the analysis, how do we expect the maximum predicted probability to differ in the testing set
# Because the CART assigns the same predicted probability to each leaf node and there are a small number of leaf nodes
# compared to data points, we expect exactly the same maximum predicted probability.

# 2.3.7 - use a threshold probability of 0.5 to answer the following
# what is the training set accuracy of the CART model?
conf.mat.train.CART = table(train$trial, predicted.prob >= 0.5)
accuracy.train.CART = sum( diag( conf.mat.train.CART ) ) / nrow(train)

# what is the training set sensitivity (true positive rate)
sens.train.CART = conf.mat.train.CART[2,2] / sum(conf.mat.train.CART[2,])

# what is the training set specificity (true negative rate)
spec.train.CART = conf.mat.train.CART[1,1] / sum(conf.mat.train.CART[1,])

# 2.4.1 - evaluate the CART model on the test set
# what is the testing set accuracy, assuming a probability threshold of 0.5?
predict.CART = predict(trials.CART, newdata = test )
conf.mat.CART = table( test$trial, predict.CART[,2] >= 0.5 )

accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow(test)

# 2.4.2 - what is the test set AUC of the prediction model?
library(ROCR)

predict.ROC = prediction( predict.CART[,2], test$trial )
performance.ROC = performance( predict.ROC, "tpr", "fpr" )
auc = as.numeric(performance(predict.ROC, "auc")@y.values)

# Section 5 is a series of questions associated with specificity and sensitivity
# Just remember,  if you are more sensitive to false negatives then DECREASE the threshold
#                 if you are more senstiive to false positives then INCREASE the threshold

#####################################
#
# Problem set 3 - Spam or Ham?
#
#####################################

#####################################
#
# Column guide
#
# text  = the text of the email
# spam  = a binary variable indicating if the email was spam
#
#####################################

# clear some space
rm( list = ls() )

# read in the data
emails = read.csv('emails.csv', stringsAsFactors = FALSE)

# 3.1.1 - how many emails are in the dataset?
nrow(emails)

# 3.1.2 - how many emails are spam?
table(emails$spam)

# 3.1.3 - which word appears at the beginning of every email in the dataset?
emails$text[[1]]

# 3.1.4 - could a spam classifier potentially benefit from including the frequency of the words that appears in every email?
# Yes, the frequency may be helpful

# 3.1.5 - how many characters are in the longest email in the dataset?
summary(nchar(emails$text))

# 3.1.6 - which row contains the shortest email in the dataset?
which.min(nchar(emails$text))

# 3.2.1 - pre-process the data into a corpus
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm    = DocumentTermMatrix(corpus)

# how many terms are dtm?
dtm

# 3.2.2 - to obtain a more reasonable dataset, remove words that don't appear in at least 5% of the documents
# how many terms are in the sparse dataset
sp.dtm = removeSparseTerms(dtm, 0.95)
sp.dtm

# 3.2.3 - build a data frame from the sparse dataset
emails.df = as.data.frame(as.matrix(sp.dtm))
colnames(emails.df) = make.names(colnames(emails.df)) # make the variable names valid

# A word stem = column and an email = rows
# what is the word stem that shows up most frequently across all emails in the dataset
which.max(colSums(emails.df))

# 3.2.4 - add a variable called "spam" to the emails.df
emails.df$spam = emails$spam

# how many words stems appear at least 5000 times in the ham emails in the dataset?
ham = (colSums(subset(emails.df, spam == 0))>=5000)
table(ham)["TRUE"]

# 3.2.5 - how many word stems appear at least 1000 times in the spam emails in the dataset?
table(colSums(subset(emails.df, spam == 1))>=1000)["TRUE"]

# 3.4.1
# build a logistic regression, CART model, and random forest attempting to predict the spam variable
# obtain the predicted probabilities on the training set

# For the ML models
emails.df$spam = as.factor(emails$spam)

# set up the datasets
set.seed(123)
split = sample.split(emails.df$spam, SplitRatio = 0.70)
train = subset( emails.df, split == TRUE )
test = subset( emails.df, split == FALSE )

# logistic regression
email.LR = glm( spam ~ . , family = "binomial", data = train )
predict.LR.train = predict(email.LR)

# CART
email.CART = rpart( spam ~ ., method = "class", data = train )
predict.CART.train = predict(email.CART)[,2]

# Random Forest
set.seed(123)
email.RF = randomForest( spam ~ ., data = train )
predict.RF.train = predict(email.RF, type = "prob")[,2]

# how many of the training set predicted probabilities from the logistic regresssion are less than 0.00001?
table(predict.LR.train < 0.00001)

# how many are greater than 0.99999?
table(predict.LR.train > 0.99999)

# how many are between 0.00001 and 0.99999
table(predict.LR.train > 0.00001 & predict.LR.train < 0.99999)

# 3.3.2 - how many variables are labeled as significant in the logistic regression model?
summary(email.LR)

# 3.3.3 - how many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
prp(email.CART)

# 3.3.4 - what is the training set accuracy of the logistic regression using a threshold of 0.5?
conf.mat.LR = table(train$spam, predict.LR.train >= 0.50)
accuracy.LR = sum( diag( conf.mat.LR ) ) / nrow(train)

# 3.3.5 - what is the training set AUC of the logisitic regression?
pred.ROCR = prediction(predict.LR.train, train$spam)
auc.LR = as.numeric(performance(pred.ROCR, "auc")@y.values)

# 3.3.6 - what is the training set accuracy of the CART model?
conf.mat.CART = table(train$spam, predict.CART.train >= 0.50)
accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow(train)

# 3.3.7 - what is the trainin set AUC of the CART model?
pred.ROCR.CART = prediction(predict.CART.train, train$spam)
auc.CART       = as.numeric(performance(pred.ROCR.CART, "auc")@y.values)

# 3.3.8 - what is the training set accuracy of the random forest, using a threshold of 0.50?
conf.mat.RF = table(train$spam, predict.RF.train >= 0.50)
accuracy.RF = sum( diag( conf.mat.RF ) ) / nrow(train)

# 3.3.9 - what is the training set AUC using the random forest model?
pred.ROCR.RF = prediction(predict.RF.train, train$spam)
auc.RF       = as.numeric(performance(pred.ROCR.RF, "auc")@y.values)

# 3.3.10 - What is the best model in terms of AUC on the training set?
# Logistic Regression > Random Forest > CART

# 3.4.1 - obtain predicted probabilities for the test set of each model
predict.LR   = predict(email.LR, newdata = test, type = "response")
predict.CART = predict(email.CART, newdata = test)[,2]
predict.RF   = predict(email.RF, newdata = test, type = "prob")[,2]

# now build the confusion matricies
conf.mat.LR   = table(test$spam, predict.LR >= 0.50)
conf.mat.CART = table(test$spam, predict.CART >= 0.50)
conf.mat.RF   = table(test$spam, predict.RF >= 0.50)

# what is the accuracy of each model?
accuracy.LR   = sum( diag( conf.mat.LR ) ) / nrow(test)
accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow(test)
accuracy.RF   = sum( diag( conf.mat.RF ) ) / nrow(test)

# what is the AUC of each model?
pred.ROCR.LR    = prediction(predict.LR, test$spam)
pred.ROCR.CART  = prediction(predict.CART, test$spam)
pred.ROCR.LR    = prediction(predict.RF, test$spam)

auc.LR    = as.numeric(performance(pred.ROCR.LR, "auc")@y.values)
auc.CART  = as.numeric(performance(pred.ROCR.CART, "auc")@y.values)
auc.RF    = as.numeric(performance(pred.ROCR.RF, "auc")@y.values)

# so in summary, the Random Forest generalized the best to the test set and the Logistic Regression over-fit the data making it the least favorable model
# One thing to consider is using a cross-validation on the CART model.  This could potentially boost its performance while simplying the complexity of the random forest
