#####################################
#
# Movie Recommendations using clustering
#
# Author: Michael Maldonado
#
#####################################

# read in the data
movies = read.table("Movie_Lens.txt", header = FALSE, sep = "|", quote = "\"")

# take a look at the structure of the data
str(movies)

# Let's add in the column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# now let's see what our data looks like
str(movies)

# Let's remove the variables we won't be using
movies$ID = NULL; movies$ReleaseDate = NULL; movies$VideoReleaseDate = NULL; movies$IMDB = NULL;

# remove duplicates
movies = unique(movies)

# now take a look at the structure of our data
str(movies)

# Quick question
# How many movies are classified as comedies?
table(movies$Comedy)

# How many movies are classified as westerns?
table(movies$Western)

# How many movies are classified as romance AND drama?
table(movies$Romance, movies$Drama)

# Let's use hierarchical clustering to cluster the movies into genres, and then make recommendations
# First we need to compute the distances between the data points
# Then we need to cluster the points

# distance computation
distances = dist(movies[2:20], method = "euclidean")

# now let's cluster our movies
cluster.movies = hclust(distances, method = "ward.D")

# the ward method cares about the distance between clusters (using the centroid method) and also the variance within those clusters

# now let's plot the dendogram
plot(cluster.movies)

# based on the dendrogram, it looks like it may be useful to choose 10 clusters
# as a note, choosing the number of clusters based on dendrograms can be challenging.  It's best to you domain specific knowledge to assist in the selection of this number.

cluster.groups = cutree(cluster.movies, k = 10)

# let's see what the clusters are like
tapply(movies$Action, cluster.groups, mean)

# this is looking at the percentage of movies in each cluster.  This can be useful to get a sense of what our groups are like

# let's try again
tapply(movies$Romance, cluster.groups, mean)

# Let's now look to see which clusters Men In Black fall into
subset(movies, Title == "Men in Black (1997)")

cluster.groups[257] # cluster 2

cluster2 = subset(movies, cluster.groups == 2)

# so which movies might be good recommendations based on interest in Men in Black?
cluster2[1:10,]

# Quick question
# Run the cutree function again, but with k = 2 this time.
cluster.groups.2 = cutree(cluster.movies, k = 2)

lapply(subset(movies, cluster.groups.2 == 2), mean) # Drama
