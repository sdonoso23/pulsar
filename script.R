library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
source("r/functions.R")

dataset<-read_csv("data/HTRU_2bis.csv")
names(dataset)<-c("X","meanip","sdip","kurtip",
                             "skewip","meandm","sddm",
                             "kurtdm","skedm")
dataset<-select(dataset,-X)

####exploratory  analysis of dataset####

#quick summary and correlation plot
summary(dataset)
cor(dataset)

#check missing and 0
colSums(is.na(dataset))
colSums(dataset==0)

normdata<-as.data.frame(scale(dataset[,]))
outliersplot<-normdata %>%
    gather(meanip:skedm,key=var,value=value) %>%
    ggplot()+geom_boxplot(aes(x=var,y=value))

outliersplot

histplots<-multiplehist(dataset)
multiplot(plotlist = histplots,cols=3)

###check relations in outliers


###scale dataset

scaledata<-as.data.frame(scale(dataset))

###pca###

pca<-prcomp(dataset,scale=TRUE)

#add pca scores to dataset
datapca<-bind_cols(dataset,as.data.frame(pca$x))

#pca plots

fviz_screeplot(pca)
fviz_pca_var(pca,col.var="contrib")


####k means####
###calculates cluster centroids and assigns each observation to the nearest
###cluster centroid

k<-c(1:10)
colnames<-c("One","Two","Three","Four","Five","Six","Seven",
          "Eight","Nine","Ten")


#get sum of square for k = 1 to 10
kmeansss<-kmeans.ss(scaledata,k)
kss<-data.frame(k=k,ss=kmeansss)
#plot of ss vs k    
ggplot(kss,aes(x=k,y=ss))+geom_line()+geom_point()
#get vectors
kmeansclusters<-kmeans.clust(scaledata,k)


#preparing data for plotting

datakmeans<-addclustdf(data = datapca,vectors = kmeansclusters,k = k,
                       colnames = colnames)

#plots
ggplot(data=datakmeans,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)
ggplot(data=datakmeans,aes(x=meandm,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

####hierarchical clustering####
##depends on the used distance matrix and the method for calculating
##cluster distance

##calculate distance matrix
distance<-dist(scaledata,method="euclidean")

hclust<-hclust(distance,method="ward.D")
plot(hclust,hang=-1,labels=FALSE)

vectors<-hclusters(distance,k,"ward.D")

#preparing data for plotting

datahclust<-addclustdf(data = datapca,vectors = vectors,k = k,
                       colnames = colnames)

#plots
ggplot(data=datahclust,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("Ward Clustering")
ggplot(data=datahclust,aes(x=meanip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

datahclust %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datahclust %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) 
    

####PAM####
###uses medoids, the most central object in the cluster or the one with
###minimum dissimilarity to all the other points in the cluster.
###dissimilarity is calculated using manhattan distance

pam1<-pam(dataset,2)

###silhouette
##calculate the average distance of a point to each of it's cluster members
##the calculate the average distance of the point to each of the points
##in other clusters
##the clusters with the min avg distance is the "neighbor cluster"
##then silhouette of the point: 
##(avg dist to neighbor cluster - avg dist to own cluster)/max between 2

#silhouette is in col 3

sil.hclust<-sil.clust(vectors,distance,k[2:10])

ggplot()+geom_line(aes(x=k[2:10],y=sil.hclust))


####nbclust

a<-NbClust(scaledata,diss=distance,distance=NULL,min.nc=2,max.nc=10,method="single",index="gap")
