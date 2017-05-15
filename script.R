library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
library(ggrepel)
source("r/functions.R")

####Load Data####

dataset<-read_csv("data/HTRU_2bis.csv")
names(dataset)<-c("X","meanip","sdip","kurtip",
                             "skewip","meandm","sddm",
                             "kurtdm","skewdm")
dataset<-select(dataset,-X)

####exploratory  analysis of dataset####

#quick summary and correlation plot
summary(dataset)
cor(dataset)

#check missing and 0
colSums(is.na(dataset))
colSums(dataset==0)


#scale data
normdata<-as.data.frame(scale(dataset[,]))

outliersplot<-normdata %>%
    gather(meanip:skewdm,key=var,value=value) %>%
    ggplot()+geom_boxplot(aes(x=var,y=value))

outliersplot

histplots<-multiplehist(dataset)
multiplot(plotlist = histplots,cols=3)

###check relations in outliers

###pca###

pca<-prcomp(dataset,scale=TRUE)

#add pca scores to dataset
datapca<-bind_cols(dataset,as.data.frame(pca$x))

#pca plots

fviz_screeplot(pca)
fviz_pca_var(pca,col.var="contrib")

####General Parameters of Analysis####
##calculate distance matrix
distance<-dist(normdata,method="euclidian")
#number of k to test
k<-c(1:6)
colnames<-c("One","Two","Three","Four","Five","Six")

####k means####
###calculates cluster centroids and assigns each observation to the nearest
###cluster centroid

#get results
kmeans<-kmeans.results(normdata,k)
#sum of squares + plot ss vs k
kms<-flatten(map(k,~kmeans[[.]][1]))
kss<-data.frame(k=k,ss=as.integer(kms))
ggplot(kss,aes(x=k,y=ss))+geom_line()+geom_point()
#clusters vectors
kmeansclusters<-as.data.frame(map(k,~kmeans[[.]][2]))

###silhouette
##calculate the average distance of a point to each of it's cluster members
##the calculate the average distance of the point to each of the points
##in other clusters
##the clusters with the min avg distance is the "neighbor cluster"
##then silhouette of the point: 
##(avg dist to neighbor cluster - avg dist to own cluster)/max between 2

#calculate silhouette, optimal k=2
sil.kmeans<-sil.clust(kmeansclusters,distance,k[2:6])
silkmeans<-data.frame(k=k[2:6],silhouette=sil.kmeans)
ggplot(data=silkmeans,aes(x=k,y=silhouette))+geom_line()+geom_point()+
    ggtitle("Silhouette K-Means")

#preparing data for plotting
datakmeans<-addclustdf(data = datapca,vectors = kmeansclusters,k = k,
                       colnames = colnames)

#summary of means by cluster
summarykmeans<-datakmeans %>%
        group_by_("k","cluster") %>%
        summarize_all(mean) %>%
        left_join(datakmeans %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
        select(1:2,19,3:18)

#plot of count
ggplot(data=summarykmeans,aes(x=k,y=count,fill=cluster))+geom_col(position="dodge")


#plots
ggplot(data=datakmeans,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("K-Means Clustering")
ggplot(data=datakmeans,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

plotlist<-datakmeans %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,17,18)) %>% 
    multiplehist2("cluster")

multiplot(plotlist=plotlist,cols=3)

####hierarchical clustering####
##depends on the used distance matrix and the method for calculating
##cluster distance

#dendrogram
hclust<-hclust(distance,method="ward.D2")
plot(hclust,hang=-1,labels=FALSE)

##calculate vectors of clusters
hclusters<-hclusters(distance,k,method="ward.D2")
##calculate silhouette
sil.hclust<-sil.clust(hclusters,distance,k[2:6])
silhclust<-data.frame(k=k[2:6],silhouette=sil.hclust)
ggplot(data=silhclust,aes(x=k,y=silhouette))+geom_line()+geom_point()+
    ggtitle("Silhouette Bottom-Up Clustering")

#preparing data for plotting

datahclust<-addclustdf(data = datapca,vectors = hclusters,k = k,
                       colnames = colnames)

#plots
ggplot(data=datahclust,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("Ward Clustering")

ggplot(data=datahclust,aes(x=meanip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#summary of means by cluster
summaryhclust<-datahclust %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datahclust %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,19,3:18)

#plot of count
ggplot(data=summaryhclust,aes(x=k,y=count,fill=cluster))+geom_col(position="dodge")

#plot of each variable with selected k
plotlist<-datahclust %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,17,18)) %>% 
    multiplehist2("cluster")

multiplot(plotlist=plotlist,cols=3)
    

####PAM####
###uses medoids, the most central object in the cluster or the one with
###minimum dissimilarity to all the other points in the cluster.
###dissimilarity is calculated using manhattan distance or euclidean

pamclust<-pam.clusters(distance,k)

#pamclust<-as.data.frame(pamclust)
#names(pamclust)<-colnames
#write_csv(pamclust,"output/pamclust.csv")

##calculate silhouette
sil.pam<-sil.clust(pamclust,distance,k[2:6])
silpam<-data.frame(k=k[2:6],silhouette=sil.pam)
ggplot(data=silpam,aes(x=k,y=silhouette))+geom_line()+geom_point()+
    ggtitle("Silhouette PAM")


datapam<-addclustdf(data = datapca,vectors = as.data.frame(pamclust),k = c(1:6),
                       colnames = colnames)

ggplot(data=datapam,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("PAM Clustering")

ggplot(data=datapam,aes(x=meanip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

summarypam<-datapam %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datapam %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,19,3:18)

#plot of count
ggplot(data=summarypam,aes(x=k,y=count,fill=cluster))+geom_col(position="dodge")

#plot of each variable with selected k

plotlistpam<-datapam %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,17,18)) %>% 
    multiplehist2("cluster")

multiplot(plotlist=plotlistpam,cols=3)




