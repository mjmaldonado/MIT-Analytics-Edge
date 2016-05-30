########################################
#
# week6 Recitation - Image recognition: segmenting images to create data
#
# Author: Michael Maldonado
#
########################################

# Grayscale Images
# Image is represented as a matrix of pixel intensity values ranging from 0 (black) to 1 (white)
# For 8 bits/pixel (bpp), 256 color levels

# The number of columns in the matrix corresponds to the width of the image
# The number of rows in the matrix corresponds to the height of the image

# When reading data into R, we need to make sure that we read it in correctly
# Our clustering algorithm will segment our color spectrum based on their intensity values.

# The input to the clustering algorithm should be a vector
# this means that we need to turn our matrix into a vector.  This is crucial!
# this means we need to lump all of the intensity values of the image into a single vector
# this can be done in R with the as.vector() function.  Once we have the vector, then we can pass it to the clustering algorithm

# We will start with hierarchical clustering
# First we need to compute the distance matrix which computes the pair-wise distance amongst the elements of the intensity vector
# So, if the matrix is n x n, we need to compute (n * (n-1))/2 pairwise distances.

# Now that we have a good background, let's do some image recognition!
# Let's read in our data
flower = read.csv('flower.csv', header = FALSE)

# based on the way the data is stored, it's not easy to tell this data is stored as a matrix of intensity values
str(flower)

# so, let's convert our data into a matrix
flower.matrix = as.matrix(flower)

# now let's take a look at the structure of the data
str(flower.matrix)

# now let's convert our matrix into a vector
flower.vector = as.vector(flower.matrix)

# take a look at the structure of the flower vector
str(flower.vector)

# now that we have our vectore, we need to compute the eucldiean distance for the hierarchical clustering algorithm
distance = dist(flower.vector, method = "euclidean")

# Use the ward method, which is a minimum variance method.  We are trying minimize the variance in the cluster, and the distance between clusters
cluster.intensity = hclust(distance, method = "ward.D")

# plot the dendrogram to help us choose the number of clusters
plot(cluster.intensity)

# We can plot the area of the clusters with the following command
rect.hclust(cluster.intensity, k = 3, border = "red")

# now let's separate our clusters
flower.clusters = cutree(cluster.intensity, k = 3)

# take a look at the average value within each cluster
tapply(flower.vector, flower.clusters, mean)

# Let's see how the image was segmented.  The fun part!
# first we need to convert our flower cluster vector into a matrix
dim(flower.clusters) = c(50, 50)

# now to reassemble the image based on our clusters
image(flower.clusters, axes = FALSE)

# let's see what the image actually looked like
image(flower.matrix, axes = FALSE, col = grey(seq(0,1,length = 256))) # turn it into grey scale

##############################################
#
# Ok, that was a good warm-up.  Let's try an MRI image!
#
##############################################

# Proceed with caution for this section.
# we will be using the "dist" function on a large vector.
# this may cause memory issues depending on how much available RAM you have

# clear some space
rm( list = ls() )

# read in the data
healthy = read.csv('healthy.csv', header = FALSE)

# convert into a matrix
healthy.matrix = as.matrix(healthy)

# now let's take a look at the structure
str(healthy.matrix) # this is quite a large image

# Let's see what the image looks like
image(healthy.matrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

# let's convert our matrix into a vector so we can compute the euclidean distances
healthy.vector = as.vector(healthy.matrix)

# uh oh, memory error!
distance = dist(healthy.vector, method = "euclidean")

# this is because we are trying to calculate ( n * (n-1) / 2 ) different distances, which in this case is 66,844,659,430!!
# this means we will not be able to use hierarchical clustering.  So, let's try a different method.
# maybe K-means will work

# K-means
# 1) Specify desired number of clusters k
# 2) Randomly assign each data point to a cluster
# 3) Compute cluster centroids
# 4) Re-assign each point to the closest cluster centroid
# 5) Re-compute cluster centroids
# 6) Repeat 4 and 5 until no improvement is made

# Let's try 5 clusters to start
k = 5

# let's use set.seed() for reproducibility
set.seed(1)

# build our clusters
KMC = kmeans(healthy.vector, centers = k, iter.max = 1000) # because this is an interative method, we need to set a maximum number of iterations

# to see the result of the algorithm, let's out put the structure
str(KMC)

# Let's grab the cluster information from the algorithm
healthy.clusters = KMC$cluster

# Let's grab the mean value information from the algorithm, too
healthy.cluster.mean = KMC$centers

# Now let's turn the cluster vector back into a matrix
dim(healthy.clusters) = c(nrow(healthy.matrix), ncol(healthy.matrix))

# Now let's visualize our clusters
image(healthy.clusters, axes = FALSE, col = rainbow(k))

# We can see that this is a fairly good staring point
# However, the real question is can we use the clusters found by the algoritm to identify tumors in another MRI image

# Plotting notes provided by the lecture presenter
# While dendrograms can be used to select the final number of clusters for Hierarchical Clustering, we can't use
# dendrograms for k-means clustering. However, there are several other ways that the number of clusters can be selected.
# One common way to select the number of clusters is by using a scree plot, which works for any clustering algorithm.

# A standard scree plot has the number of clusters on the x-axis, and the sum of the within-cluster sum of squares on the y-axis.
# The within-cluster sum of squares for a cluster is the sum, across all points in the cluster, of the squared distance between
# each point and the centroid of the cluster.  We ideally want very small within-cluster sum of squares, since this means that
# the points are all very close to their centroid. 

# To create the scree plot, the clustering algorithm is run with a range of values for the number of clusters.
# For each number of clusters, the within-cluster sum of squares can easily be extracted when using k-means clustering.
# For example, suppose that we want to cluster the MRI image from this video into two clusters. We can first run the
# k-means algorithm with two clusters:
  
  KMC2 = kmeans(healthy.vector, centers = 2, iter.max = 1000)

# Then, the within-cluster sum of squares is just an element of KMC2:
  
  KMC2$withinss

# This gives a vector of the within-cluster sum of squares for each cluster (in this case, there should be two numbers). 

# Now suppose we want to determine the best number of clusters for this dataset. We would first repeat the kmeans function call above with centers = 3, centers = 4, etc. to create KMC3, KMC4, and so on.
# Then, we could generate the following plot:
  
  NumClusters = seq(2,10,1)

  SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss), sum(KMC10$withinss))

  plot(NumClusters, SumWithinss, type="b")

# The plot looks like this (the type="b" argument just told the plot command to give us points and lines):
  
# To determine the best number of clusters using this plot, we want to look for a bend, or elbow, in the plot.
# This means that we want to find the number of clusters for which increasing the number of clusters further
# does not significantly help to reduce the within-cluster sum of squares.

# For this particular dataset, it looks like 4 or 5 clusters is a good choice.
# Beyond 5, increasing the number of clusters does not really reduce the within-cluster sum of squares too much.
  
# Note: You may have noticed it took a lot of typing to generate SumWithinss; this is because we limited ourselves to R functions we've learned so far in the course.
# In fact, R has powerful functions for repeating tasks with a different input (in this case running kmeans with different cluster sizes).
# For instance, we could generate SumWithinss with:
  
  SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
  
################################################
#
# Now let's try to identify tumors within the MRI
#
#
###############################################
  
# read in the data
tumor = read.csv('tumor.csv', header = FALSE)

# turn the data into a matrix
tumor.matrix = as.matrix(tumor)

# now turn the matrix into a vector
tumor.vector = as.vector(tumor.matrix)

# We won't re-train our data, we will use the tumor data as a test set
# first we need to install a new pacakge 'flexclust'
install.packages('flexclust')

library(flexclust)

# kcca = K-mean Centroid Cluster Analysis
KMC.kcca = as.kcca(KMC, healthy.vector)

# Now we can cluster the tumor vector based on the healthy cluster MRI
tumor.clusters = predict(KMC.kcca, newdata = tumor.vector)

# now the tumor cluster is a vector that assigns a value 1-5 based on each of the intensity values in the tumor vector, as predicted by the k-means algorithm
# let's convert the cluster vector to a matrix so we can take a look at the image

dim(tumor.clusters) = c(nrow(tumor.matrix), ncol(tumor.matrix))

# now we can visualize the clusters
image(tumor.clusters, axes = FALSE, col = rainbow(k))

# It looks like we were found the tumor!

# MRI image segmentation is subject of ongoing research

# k-means is a good starting point, but not enough
  
  # Advanced clustering techniques such as the modified fuzzy k-means (MFCM) clustering technique
  # Packages in R specialized for medical image anlaysis
  # http://cran.r-project.org/web/views/MedicalImaging.html

