##################################
#
# Week5 lectures - Text analytics
#
# Unfortunately, this dataset is not allowed to be redistributed in any way
#
# I am also forced to permanately delete this data set from my computer as soon as the course is over
#
# I'm sure it will be fun while it lasts!
#
##################################

# clear some space
rm( list = ls() )

install.packages('tm')

# read in the data
twitter.df = read.csv('tweets.csv', stringsAsFactors = FALSE) # this argument is always needed for text analytics

# take a look at the structure of the data
str(twitter.df)

# let's make a new variable for negative sentiment
twitter.df$Negative = as.factor(twitter.df$Avg <= -1)

# What proportion of our tweets are negative?
table(twitter.df$Negative)

library(tm)

install.packages('SnowballC') # this will help us use the 'tm' package
library(SnowballC)

# We need to convert our tweets into a corpus for processing
# This can be done in many different ways, but we will use the tweet column

corpus = Corpus(VectorSource(twitter.df$Tweet))
corpus[[1]]

corpus = tm_map(corpus, tolower) # make all the words lowercase - acts like tapply
corpus = tm_map(corpus, PlainTextDocument) # a corpus is a collection of documents
corpus = tm_map(corpus, removePunctuation) # get rid of punctuation

# Let's remeove stop words, too.
# Lucky for us, we are provided a database of stop words in the 'tm' package
stopwords("english")[1:10]

# We want to remove all the stop words, and also all the words Apple (as Apple is in every row of the dataset)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

# Lastly, we want to stem our document
corpus = tm_map(corpus, stemDocument)
corpus[[1]]

# We can see that this pre-processing us reduced our corpus size by about 50%

# Let's make a bag of words (a count of each word)
frequencies = DocumentTermMatrix(corpus)
frequencies

# We can see that there are 3289 words, and 1181 remaining tweets
# Let's see what this matrix looks like
inspect(frequencies[1000:1005, 505:515])

# This is a sparse matrix, meaning there are many 0's within the matrix
findFreqTerms(frequencies, lowfreq = 20) # Must show up a minimum of 20 times

# So out of our 3289 words in the matrix, only 56 appear at least 20 times in our tweets
# this means we probably have a lot of terms that will be pretty useless for our prediction model

# This implies we might have troubles with predictions using this data set for two main reasons:
# more terms = more indendent variables = more computation time = less favorable
# in building models, the ratio of indpendent variables to observations will effect how well the model will generalize

sparse = removeSparseTerms(frequencies, 0.995) # this means we only want to keep terms that appear at least .005% or more of the time
sparse # Now we can see there are only 309 terms in our new matrix

# Now let's convert the sparse matrix into a data frame that we can use for our predictive models
tweets.sparse = as.data.frame(as.matrix(sparse))

# Now let's make sure all of our words are appropriate variables names
colnames(tweets.sparse) = make.names(colnames(tweets.sparse))

# take a look out our data frame
str(tweets.sparse)

# It's important to ensure our variables are appropriate for building predictive models.
# THIS MUST BE DONE ANY TIME A DATA FRAME IS CREATED FOR TEXT PREDICTIVE MODEL IS BEING BUILT

# Let's add our Negative column to the new data frame
tweets.sparse$Negative = twitter.df$Negative

# Let's split our model into a training set and test set
library(caTools)
set.seed(123)
split = sample.split(tweets.sparse$Negative, SplitRatio = 0.7)
train = subset(tweets.sparse, split == TRUE)
test = subset(tweets.sparse, split == FALSE)

# Quick question
# Which words appear at least 100 times?
findFreqTerms(frequencies, lowfreq = 100) # Must use a matrix for this function

# Now that we've prepared our data set, let's use CART to build a predictive model
library(rpart)
library(rpart.plot)

# build the model
tweet.CART = rpart( Negative ~ ., method = "class", data = train)

# plot the model
prp(tweet.CART)

# We can see our tree makes some intuitive sense, because the three splits are typically associated with negative terms

# let's make some predictions!
predict.CART = predict(tweet.CART, newdata = test, type = "class")
conf.mat.CART = table(test$Negative, predict.CART)

# How well is our model performing in terms of accuracy?
accuracy = sum( diag( conf.mat.CART) ) / nrow(test)

# How does this compare to the baseline model (always predict non-negative)
tweets.bl = table( test$Negative )
accuracy.bl = tweets.bl[1] / nrow (test)

# So we can see our CART model does better than the baseline model

# How does a random forest compare?
library(randomForest)
set.seed(123)
# build the model
tweets.RF = randomForest(Negative ~ ., method = "class", data = train)

# make the prediction
predict.RF = predict(tweets.RF, newdata = test, type = "class")

# make a confusion matrix
conf.mat.RF = table(test$Negative, predict.RF)

# calculate the accuracy
accuracy.RF = sum( diag( conf.mat.RF ) ) / nrow(test)

# so we can see the random forest > CART > baseline in this case
# However, the interpretiblilty of the CART model, this may be the better model
# A method you could try for improving the CART model is to try cross-validation

library(caret)
set.seed(123)

# build the cross validation parameter
tr.control = trainControl(method = "cv", number = 10) # 10-folds

# build the sweeping grid
CART.grid = expand.grid(.cp = seq( 0.002, 0.1, 0.002 ) )

# train the model
tweets.CART.cv = train( Negative ~ .,
                          data = train,
                          method = "rpart",
                          trControl = tr.control,
                          tuneGrid = CART.grid
                        )

# What is the recommended cp parameter?
tweets.CART.cv # 0.034

# re-build the model with the optimized cp
tweets.CART.cp = rpart( Negative ~ ., method = "class", data = train, cp = 0.034 )

# predict on the new data
predict.CART.cp = predict( tweets.CART.cp, newdata = test, type = "class" )

# build the confusion matrix
conf.mat.CART.cp = table( test$Negative, predict.CART.cp )

# calculate the new accuracy
accuracy.CART.cp = sum( diag( conf.mat.CART.cp ) ) / nrow(test)

# What does the new model look like?
prp(tweets.CART.cp) # even simpler than the original CART model!

# So, by using cross-validation we can see that we get the same accuracy as the random forest, with an improved simplicity in the CART model
# the best of both worlds!

# One question I have, which I need to research further, is how to cross-validate a random forest.
# Does this make sense? What's the associated computation expense? With this in mind, will it be worth it?

# Quick question
# Let's see how a Logistic Regression model compares

# build the model
tweets.log.reg = glm( Negative ~., family = "binomial", data = train )

# make a prediction with the new model
predict.log.reg = predict( tweets.log.reg, newdata = test, type = "response" )

# make a confusion matrix
conf.mat.log.reg = table( test$Negative, predict.log.reg > 0.50 ) # 50% threshold

# what's the accuracy of the Logistric Regression model?
accuracy.log.reg = sum( diag( conf.mat.log.reg ) ) / nrow(test)

# This is worse than the baseline.  If we computed the accuracy on the training set, our model would perform very well
# This is an example of over-fitting.  This is a common problem for Logistic Regressions with a large number of independent variables

# The warning messages in the glm package have to do with the number of variables, and the fact that the model is overfitting the training set.
