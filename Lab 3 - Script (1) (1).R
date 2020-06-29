#------
# Author: Ignacio González Bohórquez
#Clustering Analysis
#------

library(RDSTK)#RDSTK for geocoding addresses to lat and long.
library(maps)#For drawing our maps.
library(mapdata)
library(ggplot2)
library(dplyr)#Used for data aggregation to get cluster centroids.

#Data will be the csv file being used 
#importing data from CSV file.
Data <- read.csv("namecsvfile", stringsAsFactors=FALSE)

#store names of people for later use in plots.
names <- Data$Name

#remove first column (names) so I only have numerical data now.
lab3Data<-Data[-1]

#this is how I WOULD scale data if I wanted to.
lab3Data_demo <- scale(Data,center=FALSE, scale=c(10,10,10,10,10))

#calculate pairwise euclidean distances for use in hierarchical clustering.
euclid <- dist(Data,method = "euclidean")

#run hierarchical clustering
hierarchical <- hclust(euclid, method="ward.D")

#plot dendrogram with "names" as labels.
plot(hierarchical, labels=names)

#update plot with 3-cluster solution.
rect.hclust(hierarchical,k=3,border="red")

#Let's recover cluster assignments from hclust now.
clusterResults <- cutree(hierarchical, k=3)

#Let's go back to our original dataframe, and add in cluster assignments as a new variable
Data_Hierarchical <- cbind(names,Data,clusterResults)
print(Data_Hierarchical)

#Let's calculate cluster centroids - we will get average values of each column, within each cluster
#group, using dplyr's group_by and summarize functions.
Data_Hierarchical %>% group_by(clusterResults) %>% summarise(mean(Action),mean(Comedy),mean(Drama),mean(Horror),mean(Sci.Fi))

### NOW WE'LL TRY K-MEANS
#==========================

#If I wanted kmeans to return the exact same results every time I run it with a given value for the number of clusters, I can fix the 'seed' value
#This way the system will not use randomly chosen starting (seed) values when initiating the algorithm. I'm commenting this out for now because we want to use
#random starts typically, and try it many times to ensure we're getting a "convergent" result.
# set.seed(1)

wss <- c(rep(0,nrow(Data)-1))
bss <- c(rep(0,nrow(Data)-1))

#for loop to populate WSS and BSS for every possible value of K.
for(i in 1:(nrow(Data)-1)){
  kmeans_temp <- kmeans(Data, center=i)
  wss[i] <- kmeans_temp$tot.withinss
  bss[i] <- kmeans_temp$betweenss
}

#plot WSS and BSS over values of K.
plot(1:(nrow(Data)-1),wss,type="b",col="blue")
par(new=T)
plot(1:(nrow(Data)-1),bss,type="b", axes=F, xlab="", ylab="",col="dark red")
par(new=F)

#Looks like K = 3 is the way to go.
segments <- kmeans(Data, centers=3)

# We then can look at the within cluster means for each category of film. We'll end up with 3 sets of averages here (one for each cluster).
segments$centers

# It is also possible to visualize clusters even when they are based on high-dimensional data. We can do this with what's called a Silhouette plot, as follows.
# First, calculate all pairwise euclidean distances between every combination of observations in the data. Then, square each value. 
# Then, plot the output of the "silhouette" function. 

# We need to load the cluster package to use the silhouette function.
library(cluster)

# This command will give you some warnings if you left the column of names in there, but it will drop string values when doing the calculation. 
dist_euc_sq = euclid*euclid
sil <- silhouette(segments$cluster, dist_euc_sq)

#Let's add our observation labels back in for the plot to use, and then create our plot.
rownames(sil) <- names
plot(sil)

##Problem 2
#Question: Consider the following dataset, which shows the results of 3 exams taken by 4 students (all scores are on a scale of 0-100). Which two students are the most similar to one another, according to the following distance metrics: 
#i) Euclidean
#ii) Manhattan
#iii) Max-Coordinate 
#iv) Min-Coordinate.

#Note: Min-Coordinate distance is conceptually similar to Max-Coordinate distance, it is just based on the attribute with the smallest absolute difference, rather than the attribute with the largest absolute difference. R doesn't have a native function for calculating min-coordinate distance.
Data2 <- read.csv("Data2.csv")
View(lab3Data2)

#Lets again remove the first column, because these are string values, which cant be used in our
#clustering. We'll once again store them, this time in a variable called row_names.
row_names <- Data2[1]
lab3Data2 <- data.frame(Data2[-1])

#We can use the dist() function to calculate pairwise distances using euclidean, manhattan or maximum. However, minimum distance isn't something this function
#can calculate. You would need to do it by hand, as per the slides. Same approach as max distance, but you would take the min distance across all dimensions. 
euclid <- dist(Data2,method = "euclidean",upper = TRUE)
euclid <- as.matrix(euclid)
manhat <- dist(Data2,method = "manhattan",upper = TRUE)
manhat <- as.matrix(manhat)
maxcoord <- dist(Data2,method = "maximum",upper = TRUE)
maxcoord <- as.matrix(maxcoord)

#Let's put the names back into the distance matrices.
#Note that t() is the transpose function.
colnames(euclid) <- t(row_names)
rownames(euclid) <- t(row_names)
colnames(manhat) <- t(row_names)
rownames(manhat) <- t(row_names)
colnames(maxcoord) <- t(row_names)
rownames(maxcoord) <- t(row_names)

View(euclid)
View(manhat)
View(maxcoord)

#From the above tables we can conclude the following:
#Euclidean = David-Charlotte
#Manhattan = David-Charlotte
#Max-Coord = Bill-Angela and Bill-Charlotte (tie)
#Min-Coord = David-Charlotte (this one has to be calculated by hand...). 

##Problem 3
#Question: Consider the following dataset, which shows the result of 4 medical tests conducted on 4 patients (1 positive result, 0 negative result). 
#Using the Jaccard distance metric, determine which two patients are the most similar to one another (i.e., those with the smallest distance between them) and which are the most different from one another (i.e., those with the largest distance).

# For calculating the Jaccard distance we need this package.
library(vegan)

#First we prepare a dataframe with the data provided...
df<-data.frame(matrix(c(1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1), nrow=4,ncol=4,dimnames = list(c("Archie","Barbara","Charles","Dianne"),c("Test1","Test2","Test3","Test4"))))
#Let's look at the data frame and make sure it matches the question description.
View(df)

#Now, let's use the vegdist() function from the vegan package to calculate Jaccard distance, and then view the distance matrix.
jd <- vegdist(df,method = "jaccard",upper=TRUE)
jdm <-as.matrix(jd)
View(jdm)
#We find that Archie-Charles are the closest pair (smallest distance).
#Note: we don't count a person's distance from themselves (0 on the diagonal).

##Problem 4
#Question: Assume that you have 10 stores in the city and you want to build 2 warehouses to support the store operation. 
#To find the best place for your warehouses, you decided to cluster your 10 stores into two clusters (geographically) and then place one warehouse in each cluster. 
#However, the only geographic information about the stores that you have is their postal addresses. 
#Describe how you would go about creating the dataset so that R can cluster your stores for you, and which clustering technique(s) you would use. 
#(Obviously, putting postal addresses into the spreadsheet will not work.) Assume that you can use any information that is publicly available on the 
#Web to help you with this task and that you have one hour to construct the dataset.

##Solution**
#To help along understanding how this can be done, consider a sample data set which suits our purposes. 
#It's taken from <https://www.chainstoreguide.com/p-103-apparel-store-locations.aspx> 

#To calculate a geodesic distance matrix 
library(fields)

#We need this to do some manipulation / work with string variables (e.g., concatenation)
library(stringr)

sampleStores <- read.csv("sample.csv",header = TRUE,sep = ",")
# There are more than 10 rows in this so we pick up just the first few
sampleStores <- sampleStores[2:11,]
View(sampleStores)

#The similarity calculations for such a dataset should be based on the geodesic distance between two stores. 
#Clustering should be around centrally located regions. In an actual business scenario you may have to cluster with weights 
#for sales volume and demands at each store. But we limit ourselves to simple geo clustering. Since the lattitude and longitude
#of the stores are not directly available to us we can find them out first.
##Note**: The ggmaps library used here is a tad heavy (to download and load) and you may skip this part if you could find another way to translate addresses.

#Because the addresses are spread across columns, we concatenate relevant columns into a single variable / cell (Address + City)
sampleStores$query <- str_c(sampleStores$Address.1,", ",sampleStores$City,", ",sampleStores$State," ",sampleStores$Zip)

#Let's plot where one of these addresses is located on a map.
first_address <- street2coordinates(sampleStores$query[1])
texas <- subset(map_data("state"),region %in% c("texas"))
ggplot(data=texas) + geom_polygon(aes(x=long,y=lat,group=group),fill="blue",color="white")+coord_fixed(ratio=1.3) + geom_point(aes(x=first_address$longitude,y=first_address$latitude),color="orange")

#Now let's geocode all of the addresses into latitude and longitude values, using the RDSTK.
for(i in 1:nrow(sampleStores)){
  d <- street2coordinates(sampleStores[i,8])
  sampleStores[i,'lat']<-d['latitude']
  sampleStores[i,'lon']<-d['longitude']
}

#Once we have useful numeric representations of addresses, we can now produce our distance matrix using Euclidean distance.
#Columns 9 and 10 here are latitude and longitude
euclid=dist(sampleStores[,9:10])

#Using that distance matrix, we can again run Hierarchical clustering where distance between clusters is based on cluster centroids. 
#Then, plot the dendrogram, and use the store names as labels in the plot (first column contains store name)
clust <- hclust(euclid,method="ward.D2")
plot(clust,labels=sampleStores[,1])

# The dendrogram appears to show to geographical clusters of stores in our data... let's update it with rectangles showing
# cluster membership in the 2 cluster solution.
rect.hclust(clust,k=2,border="red")
