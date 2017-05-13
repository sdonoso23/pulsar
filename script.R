library(tidyverse)
library(cluster)
library(factoextra)
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

###pca###

pca<-prcomp(dataset,scale=TRUE)

#add pca scores to dataset
datapca<-bind_cols(dataset,as.data.frame(pca$x))

#pca plots

fviz_screeplot(pca)
fviz_pca_var(pca,col.var="contrib")


####k means####

k<-c(1:10)
colnames<-c("One","Two","Three","Four","Five","Six","Seven",
          "Eight","Nine","Ten")


#get sum of square for k = 1 to 10
kmeansss<-kmeans.ss(dataset,k)
kss<-data.frame(k=k,ss=kmeansss)
#plot of ss vs k    
ggplot(kss,aes(x=k,y=ss))+geom_line()+geom_point()
#get vectors
kmeansclusters<-kmeans.clust(dataset,k)
names(kmeansclusters)<-knames


#preparing data for plotting

datakmeans<-addclustdf(data = datapca,vectors = kmeansclusters,k = k,
                       colnames = colnames)

#plots
ggplot(data=datakmeans,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)
ggplot(data=datakmeans,aes(x=meandm,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

####hierarchical clustering####

##calculate distance matrix
distance<-dist(dataset,method="euclidean")

vectors<-hclusters(distance,k,"ward.D")

#preparing data for plotting

datahclust<-addclustdf(data = datapca,vectors = vectors,k = k,
                       colnames = colnames)

#plots
ggplot(data=datahclust,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("Ward Clustering")
ggplot(data=datahclust,aes(x=meanip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)
?dist
datahclust %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datahclust %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) 
    

##silhouette
aaa<-silhouette(vectors[[3]],distance)
#silhouette is in col 3

sil.hclust<-sil.clust(vectors,distance,k[2:10])

ggplot()+geom_line(aes(x=k[2:10],y=sil.hclust))
