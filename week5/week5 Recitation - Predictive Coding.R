##########################################
#
# Week 4 - Recitation
#
# Author: Michael Maldonado
#
# Description: Analyze Enron emails with the help of predictive coding
#
##########################################

# clear some space
rm( list = ls() )

# read in the data
enron = read.csv('energy_bids.csv', stringsAsFactors = FALSE)

# take a look at the structure of the data
str(enron)

# Let's take a look at the first email
# use strwrap so the email is a lot easier to read
strwrap(enron$email[1])

# is this a responsive email for our predictive coding?
enron$responsive[1] # no

# how about the second email?
strwrap(enron$email[2])

# is this a responive variable?
enron$responsive[2]

# How many of the emails are reponsive (I can use this for the baseline, too)?
email.bl = table(enron$responsive)
email.bl[2] / nrow(enron)

# Now let's build our corpus
library(tm)
corpus = Corpus(VectorSource(enron$email))

# make sure this worked as intended
strwrap(corpus[[1]])

# Now start the pre-processing
# make all characters lower case
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)

# remove the punctuation
corpus = tm_map(corpus, removePunctuation)

# remove the stop words
corpus = tm_map(corpus, removeWords, stopwords("english"))

# stem the document
corpus = tm_map(corpus, stemDocument)

# Now take a look at the first email
strwrap(corpus[[1]])

# Now let's build a document term matrix
dtm = DocumentTermMatrix(corpus)
dtm

# Let's remove the terms that don't appear very often
dtm = removeSparseTerms(dtm, 0.97) # remove words that don't appear in at least 3% of the document
dtm

# this is much better, we now have 788 terms vs over 22,000

# let's turn our document term matrix into a data frame
labeled.terms = as.data.frame(as.matrix(dtm))

# Let's add our responsive variable
labeled.terms$responsive = enron$responsive

# take a look at the new data frame
str(labeled.terms)

# let's separate the data into a test and training set
library(caTools)
set.seed(144)
split = sample.split(labeled.terms$responsive, SplitRatio = 0.70)
train = subset(labeled.terms, split == TRUE)
test = subset(labeled.terms, split == FALSE)

# let's build our model now, using CART
library(rpart)
library(rpart.plot)

email.CART = rpart( responsive ~ ., method = "class", data = train )

# take a look at the tree
prp(email.CART)

# evaluate the model
predict.email = predict( email.CART, newdata = test )

# we would like to know the predicted probability that an email is repsonsive so let's obtain this variable
predict.prob = predict.email[,2]

# we would like to know the accuracy of our prediction, so let's take a look
conf.mat.CART = table( test$responsive, predict.prob >= 0.5 )
conf.mat.CART

accuracy.CART = sum( diag( conf.mat.CART ) ) / nrow(test)

# how does this compare to the baseline?
accuracy.bl = table(test$responsive)[1] / nrow(test)

# So we can see the CART model gives as a small improvement in accuracy

# due to the type of prediction we are making, we should be penalized more for a false negative than a false positive
# this means we will have to modify our threshold to be lower

library(ROCR)
pred.ROCR = prediction(predict.prob, test$responsive)
perf.ROCR = performance(pred.ROCR, "tpr", "fpr")
plot(perf.ROCR, colorize = TRUE)

# let's take a look at the AUC
auc = performance(pred.ROCR, "auc")@y.values
auc # this means it can discern between a reponsive and non-responsive email about 80% of the time
