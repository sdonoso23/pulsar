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

#get sum of square for k = 1 to 10
kmeansss<-kmeans.ss(dataset,k)
kss<-data.frame(k=k,ss=kmeansss)
#plot of ss vs k    
ggplot(kss,aes(x=k,y=ss))+geom_line()+geom_point()
#get vectors
kmeansclusters<-kmeans.clust(dataset,k)
knames<-c("One","Two","Three","Four","Five","Six","Seven",
          "Eight","Nine","Ten")
names(kmeansclusters)<-knames

#preparing data for plotting
datakmeans<-datapca %>%
    bind_cols(kmeansclusters) %>%
    gather(One:Ten,key=k,value=cluster)
    
datakmeans$cluster<-factor(datakmeans$cluster,levels=c(1:10))
datakmeans$k<-factor(datakmeans$k,levels=knames)

#plots
ggplot(data=datakmeans,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)
ggplot(data=datakmeans,aes(x=meandm,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

###hierarchical clustering###

##calculate distance matrix
distance<-dist(dataset,method="euclidean")
hclust()

vectors<-clustvectors(distance,k,"ward.D")

knames<-c("One","Two","Three","Four","Five","Six","Seven",
          "Eight","Nine","Ten")
names(vectors)<-knames

#preparing data for plotting
datahclust<-datapca %>%
    bind_cols(vectors) %>%
    gather_(key="k",value="cluster",gather_cols = knames)


datahclust$cluster<-factor(datahclust$cluster,levels=c(1:10))
datahclust$k<-factor(datahclust$k,levels=knames)

#plots
ggplot(data=datahclust,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)
ggplot(data=datahclust,aes(x=meanip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

datahclust %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datahclust %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) 
    

