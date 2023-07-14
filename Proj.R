#Customer Segmentation in R implementing K-means algorithm

getwd()

setwd("C:/Users/parip/OneDrive/Desktop/Customer Segmentation")

#Loading the data

customer_data <- read.csv("Mall_Customers.csv")
customer_data

#Quick summary of data

head(customer_data)
summary(customer_data$Age)

#Some statistical value of features

sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)

sd(customer_data$Spending.Score..1.100.)

#Plot gender visualization

a=table(customer_data$Gender)
barplot(a,main="Using Barplot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

#Piechart

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,main="Pie Chart depicting ratio of male and female")

#Age Graph Histogram

summary(customer_data$Age)
hist(customer_data$Age,
     col="green",
     main="Histogram of show count of age class",
     xlab="Age Class",
     ylab="Frequency",
     labels = TRUE)

#Boxplot for age

boxplot(customer_data$Age,
        horizontal = TRUE,
        col="green",
        main="Boxplot for analysis of age")

#Annual income

summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="#660033",
     main="Histogram for annual income",
     xlab="Annual income class",
     ylab = "Frequency",
     labels = TRUE)

plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density plot for annual income",
     xlab="Annual income class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#ccff66")

#Spending Score

summary(customer_data$Spending.Score..1.100.)

boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col="#990000",
        main="Boxploy for spending score")

hist(customer_data$Spending.Score..1.100.,
     main="Histogram for spending score",
     xlab = "Spending Score Class",
     ylab = "Frequency",
     col="#6600cc",
     labels = TRUE)

#K-means Clustering

#Elbow method : intra-cluster variation stays minimum

library(purrr)
set.seed(123)

#Function to calculate total intra-cluster sum of square
iss <- function(k) 
{
  kmeans(customer_data[,3:5],k,iter.max = 100,nstart=100,algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values,iss)

plot(k.values, iss_values,
     type="b",pch=19,frame=FALSE,
     xlab="Number of clusters K",
     ylab = "Total intra-clusters sum of squares")

#Clusters
#Average silhoouette method

library(cluster)
library(gridExtra)
library(grid)

k2<-kmeans(customer_data[,3:5],2,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))


#Determine and visualize the optimal number of clusters

library(NbClust)
library(factoextra)

fviz_nbclust(customer_data[,3:5],kmeans,method="silhouette")

#Gap static method

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10 , B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(customer_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
k6

#Visualizing the clustring results using the first two principle components

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #Principal Component Analysis
summary(pcclust)

pcclust$rotation[,1:2]

#Visualize

set.seed(1)
ggplot(customer_data, aes(x=Annual.Income..k..,y=Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color=as.factor(k6$cluster))) + 
  scale_color_discrete(name=" ",
                       breaks=c("1","2","3","4","5","6"),
                       labels=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6")) + 
  ggtitle("Segments of mall customers",subtitle = "Using K-means Clustering")

#K-means
ggplot(customer_data, aes(x=Spending.Score..1.100.,y=Age)) + 
  geom_point(stat = "identity", aes(color=as.factor(k6$cluster))) + 
  scale_color_discrete(name=" ",
                       breaks=c("1","2","3","4","5","6"),
                       labels=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5","Cluster 6")) + 
  ggtitle("Segments of mall customers",subtitle = "Using K-means Clustering")

#Finally visulaize K-means values

kcols = function(vec)
{
  cols=rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}

digCluster <- k6$cluster; dignm<-as.character(digCluster);

plot(pcclust$x[,1:2], col=kcols(digCluster),pch=19,xlab = "K-means",ylab = "Classes")
legend("bottomleft",unique(dignm),fill=unique(kcols(digCluster)))

