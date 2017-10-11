
############################## Note on scaling
# In hierarchical clustering we have two types of scaling: Standard Scaler and MinMaxscaler
# standard scaler is calculated by (x-mean)/std and result will be between -3.4 to 3.4 usually.
# MinMax scaler is calculated by (x-min(x))/(max(x)-min(x)) and result will be between 0 to 1.
# When we have a categorical feature, we have to dummitize that column and as a result we have columns
# with values of 0 or 1. In these kind of cases that we have mix data (numric + categorical), using 
# MinMax scaler is highly recommended, because all columns will be between 0 to 1.

# In case of mixed data, if we don`t do one hot encoding and dummitize the data, we can use gower distance
# When we have mixed data, we can use euclidean distance and we need to calculate the gower distance:
# gower_dist <- daisy(data[, -1],
#                    metric = "gower",
#                    type = list(logratio = 3))
#Source: https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

###################### Data
#In order to visualize the data we select 2 features
x <- runif(50) #genereate 50 random number
y <- runif(50)

data <- cbind(x,y)
data

###################### plot the data
plot(data,type='n') #remove dots
text(data,rownames(data)) #put rownumbers instead of dots on the plot


###################### select the clusters randomly
#Randomly select 4 clusters:
km <- kmeans(data,centers = 4)
str(km)
#tot.withinss is the within cluster sum of squares




###################### Specify the number of clusters Elbow Method (wcss)
#Use the Elbow Method (wcss):
#for wcss we need to calculate the sum of squares in each cluster
#kmeans returns an object which has "tot.withinss" component
#tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss
wcss = c()
for (i in 1:10){
  wcss[i] <- kmeans(data,centers=i)$tot.withinss
}

#Plot the value
plot(1:10,wcss,type='b',
     main="K-Means Clustering Scree-Plot",
     xlab="Number of Clusters",ylab="Within groups sum of squares")


#Based on the graph, we have 4 clusters (elbow)
#iter.max: the maximum number of iterations allowed.
#nstart:if centers is a number, how many random sets should be chosen?
set.seed(1234)
kmeans_result <- kmeans(data,centers = 4,iter.max=300)

###################### Visualizing the clusters:
library(cluster)
clusplot(data,clus = kmeans_result$cluster,
         lines = 0,shade=TRUE,color=TRUE,labels = 2, plotchar = FALSE, span = TRUE,
         main = 'Clusters')


#Show the animation of selecting clusters:
#install.packages("animation")
library(animation)
km1 <- kmeans.ani(data,4)


###################### Specify the number of clusters kselection
#install.packages("kselection")
library(kselection)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k #2clusters


#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k



###################### Clustering large data
library(cluster)
dataset <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
clusters_clara <- clara(dataset,k=2,samples = 100)
clusters_clara
clusplot(clusters_clara)


#Second way (pam is much slower than clara):
#pam --> Partitioning Around Medoids
clusters_pam <- pam(dataset,k=2)
clusplot(clusters_pam)
# Medoids are representative objects of a data set or a cluster with a data set whose average dissimilarity to all
# the objects in the cluster is minimal. Medoids are similar in concept to means or centroids, but medoids are 
#always members of the data set. Medoids are most commonly used on data when a mean or centroid cannot be defined,
#such as for 3-D trajectories or in the gene expression context.
#Source: https://en.wikipedia.org/wiki/Medoid















