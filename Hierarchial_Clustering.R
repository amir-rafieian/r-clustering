
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


############################## Reading Data 
#install.packages("xlsx")
library(xlsx)

input <- read.xlsx('University_Clustering.xlsx',sheetIndex=1)
dataset <- input[1:25,c(1,3:8)]


############################## Normalize the data
# Excluding the university name column before normalizing the data
scaled_dataset <- scale(dataset[c(2:7)]) 


############################## Distance Matrix & Clustering
# d=distance matrix of our dataset which is a matrix contains the euclidean distanct between each pair
dist_matrix <- dist(scaled_dataset,method = "euclidean")
hc = hclust(d=dist_matrix,method = "complete")
# method = "complete" => based on the maximum distance between two cluster
# method = "ward.D" tries to minimize the variance within each cluster 

############################## Display dendogram
plot(hc,hang = -1, #hang=-1 => shows all the entries at the same level
     main = "Dendogram",
     xlab ='Universities', ylab='Distance') 


############################## Cutting Trees
#Based on the graph, we have decided to have 5 clusters
#we make a vector that tells us each university belongs to which cluster
#Actually the recommended approach to select the number of clusters is to select the longest vertical line
#that wouldn`t cross by any horizontal line hyptechnically. (in this case it would be 2 cluster)
#Another way is to set a threshold for distance and select the number of clusters based on that (relaed to domain knowledge)
uni_group <- cutree(tree = hc,k=5)

############################## Visualizing the clusters
#Draw rectangles around the branches of a dendrogram highlighting the corresponding clusters
rect.hclust(tree=hc,k=5,border = "red")


#Visualising the clusters:
library(cluster)
clusplot(x = scaled_dataset,clus = uni_group,
         lines = 0,shade=TRUE,color=TRUE,labels = 2, plotchar = FALSE, span = TRUE,
         main = 'Clusters of Universities')


############################## Prepare the final data & Data mining
final_df <- data.frame(dataset,uni_group)
#put the uni_group at the first column
library(dplyr)
final_df %>% select('uni_group',everything())

final_df[,2:8] %>% group_by(uni_group) %>% summarise_all(funs(mean))
#or
aggregate(final_df[,2:8],by=list(uni_group),FUN=mean)
#It shows students in universities cluster 2, have the highest SAT score, 90.5% of the
#are in the top-10 list of students, Acceptance ratio is very low (only 24.3%), Graduation rate
#is also very high (92.3%). Now we check the final_df to see which universities are in the
#cluster 2

#write to an excel file
write.xlsx(x=final_df,file='university_hc.xlsx')

