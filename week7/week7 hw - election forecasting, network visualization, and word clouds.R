################################################
#
# week 7 homework
#
# Author: Michael Maldonado
#
################################################

################################################
#
# Problem Set 1 - Election Forecasting
#
################################################

library(ggplot2)
library(maps)
library(ggmap)

US.map = map_data('state')

# 1.1.1 - How many different groups are there?
unique(US.map$group)

# 1.1.2 - Plot a map of the United States
ggplot(US.map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# Which command from above in geom_polygon defines the color of the outline of the states?

# Now, let's color the map of the US according to our 2012 US presidential election predctions.
poll = read.csv('pollingImputed.csv')

str(poll)

# split the data into a training and test set
train = subset(poll, Year == 2004 | Year == 2008)
test  = subset(poll, Year == 2012)

str(test) # we are missing data from Alaska, Delaware, Alabama, Wyoming, and Vermont, so these states willnot appear colored in our map

# build a logistic regression model to predict whether a state votes republican or not
poll.LR = glm(Republican ~ SurveyUSA + DiffCount, family = "binomial", data = train)

# now see how this prediction generalizes
pred.LR = predict(poll.LR, newdata = test, type = "response")

# let's create a vector of Republican/Democrat predictions and then turn it inot a data frame for mapping with ggplot
pred.LR.binary = as.numeric(pred.LR > 0.5)
pred.df = data.frame(pred.LR, pred.LR.binary, test$State)

# how many states predict a republican election
nrow(subset(pred.df, pred.LR.binary == 1))

# what is the average predicted probability of our model
mean(pred.df$pred.LR)

# 1.2.2
# merge pred.df with US.map
# first we need to convert the pred.df$test.state to lowercase so that it matches with US.map
pred.df$region = tolower(pred.df$test.State)

# now we can merge
pred.map = merge(US.map, pred.df, by = 'region')

# now we need to order our observations to ensure the map gets drawn properly
pred.map = pred.map[order(pred.map$order),]

# how many observations are in pred.map?
# use RStudio "Global Environment" to answer

# how many observations are in US.map?
# use RStudio "Global Environment" to answer

# why did the number of observations change?
# it has to do with not having all states in the test set (merge performs an INNER JOIN by default)

# now let's color the US map with our predictions!
ggplot(pred.map, aes(x = long, y = lat, group = group, fill = pred.LR.binary)) + geom_polygon(color = "black")

# which color corresponds to a Republican prediction?
# whichever color is closest to 1

# let's clean up our plot a little by changing the color of the democratic and republican predications, as well as the prediction legend
ggplot(pred.map, aes(x = long, y = lat, group = group, fill = pred.LR.binary)) + geom_polygon(color = "black", alpha = 0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks = c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# if you change pred.LR.binary to pred.LR (the binary predictions vs the predicted probabilities) you'll notice the maps appear to be very similar
# this is do the most of the predicted probabilities being close to either 0 or 1

# 1.3.1
# what is the predicted outcome for the state of Florida?
# 1

# what was our predicted probability for the state of Florida?
subset(pred.df, test.State == "Florida")

# What does this imply?
# our preciction was way off, and we were very confident in this bad prediction based on the high probability

# 1.4.1
# what is the name of the parameter to change the line type? geom_polygon(linetype)
# what is the name of the parameter to change the line thickness? geom_polygon(size)

# 1.4.2
# what is the name of the parameter to change the transparency of the colors? geom_polygon(alpha)

#######################################################################
#
# Problem set 2 - Visualizing Social Networks
#
#######################################################################

# clear some space
rm( list = ls() )

# read in the data
edges = read.csv('edges.csv'); users = read.csv('users.csv')

# edges contains variables V1 and V2, which label the endpoints of edges in our network.  Each row represents a pair of users in our graph who are Facebook freinds.
# for a pair of friends A and B, edges will only contain a single row -- the smaller identifier will be listed first in this row.  From this row,
# we will know that A is freinds with B and B is friends with A

# users contains information about the Facebook users, who are the vertices in our network

################################################
#
# users Column guide
#
# id      = a unique identifier for this user,; this is the value that appears in the rows of edges.csv
# gender  = An identifier for the gender of a user taking the values A and B.  Because the data is anonymized, we don't know which value referes to males and which value refers to females
# school  = An identifier for the school the user attended taking the values A and AB (users with AB attended school A as well as another school B).  Because the data is anonymized, we don't kno wthe schools represtned by A and B
# locale  = An identifier for the locale of the user taking the values A and B.  Because the data is anonymized, we don't know which value refers to what locale.
#
################################################

# 2.1.1 - how many Facebook users are there in our dataset?
str(users)

# in our dataset, what is the average number of friends per user?
str(edges)

sum(table(edges$V2)) / nrow(users) * ncol(edges) # multiply by the numbers of users

# 2.1.2 - out of all students who listed a school, what was the most common locale?
table(users$school, users$locale)

# 2.1.3 - is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school)

# install the package igraph to help visualize our networks
install.packages('igraph')
library(igraph)

# 2.2.1 - which command will create a graph g describing our social networks, with attributes of each user correctly loaded?
?graph.data.frame

# in this case, the directed parameter is set to FALSE beccause if A is a Facebook friend of B then B is a Facebook friend of A.

# 2.2.2 - create a graph data frame and plot the results (removing the text labels and smaller vertices)
g = graph.data.frame(edges, FALSE, users)

plot(g, vertex.size = 5, vertex.label = NA)

# 2.2.3 - how many users are friends with 10 or more other Facebook users in this network?
table( degree(g) >= 10 )

# 2.2.4
# In a network, it's often visually helpful to draw attention to "important" nodes in the network.  While this might mean
# different things in different contexts, in a social network we might consider a user with a large number of friends to be an important
# user.  From the previous problem, we know this is the same as saying that nodes with a high degreee are important users.

# Let's change the size of each node which represnets an "important" user
V(g)$size = degree(g)/2 + 2

# now replot the data
plot(g, vertex.label = NA)

# what is the largest size we assigned to any node in our graph?
max(degree(g))/2 + 2

# what is the smallest size we assigned to any node in our graph?
min(degree(g))/2 + 2

# 2.3.1 - Coloring Vertices
# Update the colors of the vertices by setting the color to black for all vertices, then setting it to red for the vertices with gender A and setting it to gray for the verties with gender B:
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label = NA)

# 2.3.2 - now color the vertices based on the school that each user in our network attended.
# are the two users who attended both school A and B Facebook friends with each other?
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label = NA)

# 2.3.3 - now color the vertices based on the locale of the user.
# The large connected component is most associated with which locale?
# The 4-user connected component is most associated with which locale?
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label = NA)

# 2.4.1 - which igraph.plotting function would enable us to plot our graph in 3-D?
?igraph.plotting

install.packages('rgl')

rglplot(g, vertex.label = NA) # cool :)

# what parameter to the plot() function would we use to change the edge width when plotting g?
plot(g, vertex.label = NA, edge.width = 2)

##################################################
#
# Problem set 3 - Twitter word clouds
#
##################################################

# clear some space
rm( list = ls() )

# read in the data
tweets = read.csv('tweets.csv', stringsAsFactors = FALSE)

##################################################
#
# Column guide
#
# Tweet = the text of the tweet
# Avg   = the sentiment of the tweet, as assigned by users of Amazon Mechanical Turk.  The score ranges on a scale from -2 to 2, where 2 means highly positive sentiment,
#         -2 means highly negative sentiment, and 0 means neutral sentiment
#
##################################################

# pre-process the data
library(tm)

# 3.1.1
# create a document corpus
corpus = Corpus(VectorSource(tweets$Tweet))
# make all words lowercase
corpus = tm_map(corpus, tolower); corpus = tm_map(corpus, PlainTextDocument)
# remove punctuation
corpus = tm_map(corpus, removePunctuation)
# remove stop words
corpus = tm_map(corpus, removeWords, stopwords(kind = "en"))
# turn the corpus into a document term matrix
dtm = DocumentTermMatrix(corpus)
# turn the document term matrix into a data frame for analysis
all.tweets = as.data.frame(as.matrix(dtm))

# how many uniqe words are there across all documents?
ncol(all.tweets)

# 3.1.2
# why didn't we stem the words in this corpus?
# It would make the words in the word cloud harder to read and understand otherwise

# 3.2.1
# install and load the "wordcloud" package
install.packages('wordcloud');
library(wordcloud)

# as we can see from ?wordcloud, we will need to provide the function with a vector of words and a vector of word frequencies.
# which function can we apply to all.tweets to get a vector of the words in our dataset?
# str, rownames, or colnames?

# 3.2.2 -  Which function should we apply to all.tweets to obtain the frequency of each word across all tweets?
# colSums, rowSums, or sum?

# 3.2.3 - Create the wordcloud
# what is the most common word accross all tweets?
sort(colSums(all.tweets))

wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5))

# 3.2.4 - repeat the pre-processing, but this time remove the most frequenly occuring word
# create a document corpus
corpus = Corpus(VectorSource(tweets$Tweet))
# make all words lowercase
corpus = tm_map(corpus, tolower); corpus = tm_map(corpus, PlainTextDocument)
# remove punctuation
corpus = tm_map(corpus, removePunctuation)
# remove stop words and the most commonly occuring word
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# turn the corpus into a document term matrix
dtm = DocumentTermMatrix(corpus)
# turn the document term matrix into a data frame for analysis
all.tweets = as.data.frame(as.matrix(dtm))

# create a word cloud with the update corpus
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5))

# what is the most common word now?
# iphone

# 3.3.1, 3.3.2, 3.3.3, 3.3.4, 3.3.5
# which word cloud is based only on negative tweets?
# which word cloud was created without modifying the parameters min.freq or max.words.
# which word clouds were created with parameter random.order set to false?
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), random.order = FALSE)

# which word cloud was built with a non-default value for parameter rot.per?
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), rot.per = 0.5)

# which world cloud was random.color set to TRUE?
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), random.color = TRUE)

# 3.4.1
# The use of a palette of colors can often imporve the overall effect of a visualiztion.  We can easily select our
# own colors when plotting; for instance, we could pass c("red", "green", "blue") as the color parameter to wordcloud().  The RColorBrewer
# package, which is based on the ColorBrewer project (colorbrewer.org), provide pre-selected palettes that can lead to more visually appealing images.
# though these palettes are designed specifically for coloring maps, we can also use them in our word clouds and other visualizations.

# install the RColorBrewer package
install.packages('RColorBrewer');
library(RColorBrewer)

# which color palette would be most appropriate for use in a word cloud for which we want to use color to indicate word frequency?
display.brewer.all()
# the more monochromatic the better in this case

# 3.4.2
# In sequential palattes, sometimes there is an undesirably large contrast betwen the lightest and darkest colors.
# We can see this effect when plotting a word cloud for all.tweets with parameter colors = brewer.pal(9,"Blues"), which returns a sequential blue palette with 9 colors
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), colors = brewer.pal(9,"Blues"))

# which command will address this issue by removing the first 4 elements of the 9-color palette of blue colors?
# brewer.pal(9, "Blues")[c(-1,-2,-3,-4)]
# brewer.pal(9, "Blues")[c(5,6,7,8,9)]
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), colors = brewer.pal(9,"Blues")[c(5,6,7,8,9)])
wordcloud(colnames(all.tweets), colSums(all.tweets), scale = c(3,0.5), colors = brewer.pal(5,"GnBu"))
