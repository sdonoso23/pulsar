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


#exploratory plots
outliersplot<-normdata %>%
    gather(meanip:skewdm,key=var,value=value) %>%
    ggplot()+geom_boxplot(aes(x=var,y=value))

ggsave("plots/boxplotcompletedataset.jpg",outliersplot)

histplots<-multiplehist(dataset)
ggsave("plots/densitycompletedataset.jpg",multiplot(plotlist = histplots,cols=3))

###check relations in outliers

lista<-list()

for (i in 1:8){
    
    iqr<-IQR(dataset[[i]])
    quantiles<-quantile(dataset[[i]],c(0.25,0.75))
    
    lista[[i]]<-ifelse(dataset[i]<quantiles[1]-1.5*iqr | dataset[i]>quantiles[2]+1.5*iqr,
                       TRUE,FALSE)
}

outliers<-as.data.frame(lista)
names(outliers)<-paste(names(dataset),"outliers",sep="")

outliers<-outliers %>%
    mutate(noutliers=rowSums(outliers))

table(outliers$noutliers)
colSums(outliers[1:8])

###pca###

pca<-prcomp(dataset,scale=TRUE)

#add pca scores to dataset
datapca<-bind_cols(dataset,as.data.frame(pca$x),as.data.frame(outliers$noutliers))
a<-names(datapca)
a[17]<-"noutliers"
names(datapca)<-a

#pca plots

ggsave("plots/pcavariance.jpg",fviz_screeplot(pca))
ggsave("plots/pcavariablecontribution.jpg",fviz_pca_var(pca,col.var="contrib"))

####General Parameters of Analysis####
##calculate distance matrix
distance<-dist(normdata,method="euclidean")
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
silkgraph<-ggplot(data=silkmeans,aes(x=k,y=silhouette))+geom_line()+geom_point()+
    ggtitle("Silhouette K-Means")
#plot of silhouette
ggsave("plots/kmeansil.jpg",silkgraph)

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
        select(1:2,20,3:19)



#plots

#count of obs in clusters
ggplot(data=summarykmeans,aes(x=k,y=count,fill=cluster))+geom_col(position="dodge")


##k=2 vs outliers
p1<-datakmeans %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=noutliers))+geom_point()+
    ggtitle("Outliers")

p2<-datakmeans %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("K-Means K=2")

ggsave("plots/kmeansout.jpg",p1)
ggsave("plots/kmeans2clusters.jpg",p2)
ggsave("plots/kmeansoutliers.jpg",multiplot(p1,p2,cols=2))

##clustering from k=1 to k=6
kmeansclustersgraph<-ggplot(data=datakmeans,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("K-Means Clustering")

ggsave("plots/kmeansclusters.jpg",kmeansclustersgraph)

##densities from k=1 to k=6
ggplot(data=datakmeans,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#cluster k=2 and k=3
k2<-datakmeans %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("K-Means K=2")

k3<-datakmeans %>%
    filter(k=="K=3") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("K-Means K=3")

ggsave("plots/kmeansk2k3.jpg",multiplot(k2,k3,cols=2))


#densities for all variables k=2 and k=3
plotlistkmeans2<-datakmeans %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/kmeansdensityk2.jpg",
       multiplot(plotlist=plotlistkmeans2,cols=3))

plotlistkmeans3<-datakmeans %>% 
    filter(k=="K=3") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/kmeansdensityk3.jpg",
       multiplot(plotlist=plotlistkmeans3,cols=3))

####hierarchical clustering####
##depends on the used distance matrix and the method for calculating
##cluster distance

#dendrogram
hclust<-hclust(distance,method="ward.D2")
jpeg("plots/dendrogramward.jpg")
plot(hclust,hang=-1,labels=FALSE)
dev.off()

##calculate vectors of clusters
hclusters<-hcluster(distance,k,method="ward.D2")
##calculate silhouette
sil.hclust<-sil.clust(hclusters,distance,k[2:6])
silhclust<-data.frame(k=k[2:6],silhouette=sil.hclust)
ggsave("plots/wardsil.jpg",ggplot(data=silhclust,aes(x=k,y=silhouette))+
           geom_line()+geom_point()+
    ggtitle("Silhouette Ward Clustering"))

#preparing data for plotting

datahclust<-addclustdf(data = datapca,vectors = hclusters,k = k,
                       colnames = colnames)

#summary by cluster
summaryhclust<-datahclust %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datahclust %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,20,3:19)


#plots

#count of obs in clusters
ggplot(data=summaryhclust,aes(x=k,y=count,fill=cluster))+
    geom_col(position="dodge")

#k=2 vs outliers
hout1<-datahclust %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=noutliers))+geom_point()+
    ggtitle("Outliers")

hout2<-datahclust %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Ward Clustering K=2")

ggsave("plots/wardout.jpg",hout1)
ggsave("plots/ward2clusters.jpg",hout2)
ggsave("plots/wardoutliers.jpg",multiplot(hout1,hout2,cols=2))

##clustering from k=1 to k=6
hclustclustersgraph<-ggplot(data=datahclust,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("Ward Clustering")

ggsave("plots/wardclusters.jpg",hclustclustersgraph)

##densities from k=1 to k=6
ggplot(data=datahclust,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#cluster k=2 and k=3
h2<-datahclust %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Ward K=2")

h3<-datahclust %>%
    filter(k=="K=3") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Ward K=3")

ggsave("plots/wardk2k3.jpg",multiplot(h2,h3,cols=2))


#densities for all variables k=2 and k=3
plotlisthclust2<-datahclust %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/warddensityk2.jpg",
       multiplot(plotlist=plotlisthclust2,cols=3))
    
plotlisthclust3<-datahclust %>% 
    filter(k=="K=3") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/warddensityk3.jpg",
       multiplot(plotlist=plotlisthclust3,cols=3))



####PAM####
###uses medoids, the most central object in the cluster or the one with
###minimum dissimilarity to all the other points in the cluster.
###dissimilarity is calculated using manhattan distance or euclidean

#load data because it takes too much time
#pamclust<-pam.clusters(distance,k)
pamclust<-read_csv("output/pamclust.csv")

#pamclust<-as.data.frame(pamclust)
#names(pamclust)<-colnames
#write_csv(pamclust,"output/pamclust.csv")


##calculate silhouette
sil.pam<-sil.clust(pamclust,distance,k[2:6])
silpam<-data.frame(k=k[2:6],silhouette=sil.pam)
ggsave("plots/pamsil.jpg",ggplot(data=silpam,aes(x=k,y=silhouette))+geom_line()+geom_point()+
    ggtitle("Silhouette PAM"))


#preparing data for plotting
datapam<-addclustdf(data = datapca,vectors = as.data.frame(pamclust),k = c(1:6),
                       colnames = colnames)

#summary by cluster
summarypam<-datapam %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(datapam %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,20,3:19)


#plots
#plot of count
ggplot(data=summarypam,aes(x=k,y=count,fill=cluster))+
    geom_col(position="dodge")

#k=2 vs outliers
pamout1<-datapam %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=noutliers))+geom_point()+
    ggtitle("Outliers")

pamout2<-datapam %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("PAM Clustering K=2")

ggsave("plots/pamout.jpg",pamout1)
ggsave("plots/pam2clusters.jpg",pamout2)
ggsave("plots/pamoutliers.jpg",multiplot(pamout1,pamout2,cols=2))

##clustering from k=1 to k=6
pamclustersgraph<-ggplot(data=datapam,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("K-Medoids Clustering")

ggsave("plots/pamclusters.jpg",pamclustersgraph)

##densities from k=1 to k=6
ggplot(data=datapam,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#cluster k=2 and k=3
pk2<-datapam %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("K-Medoids K=2")

pk3<-datapam %>%
    filter(k=="K=3") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("K-Medoids K=3")

ggsave("plots/pamk2k3.jpg",multiplot(pk2,pk3,cols=2))


#densities for all variables k=2 and k=3
plotlistpam2<-datapam %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/pamdensityk2.jpg",
       multiplot(plotlist=plotlistpam2,cols=3))

plotlistpam3<-datapam %>% 
    filter(k=="K=3") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/pamdensityk3.jpg",
       multiplot(plotlist=plotlistpam3,cols=3))



####clara####

#get clusters
claraclusters<-clara.clusters(normdata,k,"euclidean")
#calculate silhouette
sil.clara<-sil.clust(claraclusters,distance,k[2:6])
silclara<-data.frame(k=k[2:6],silhouette=sil.clara)
ggsave("plots/clarasil.jpg",ggplot(data=silclara,aes(x=k,y=silhouette))+geom_line()+geom_point()+
           ggtitle("Silhouette CLARA"))

#preparing data for plottting
dataclara<-addclustdf(data = datapca,vectors = claraclusters,k = k,
                    colnames = colnames)

#summary by clusters
summaryclara<-dataclara %>%
    group_by_("k","cluster") %>%
    summarize_all(mean) %>%
    left_join(dataclara %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,20,3:19)

#plots
#plot of count
ggplot(data=summaryclara,aes(x=k,y=count,fill=cluster))+
    geom_col(position="dodge")

#k=2 vs outliers
claraout1<-dataclara %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=noutliers))+geom_point()+
    ggtitle("Outliers")

claraout2<-dataclara %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("CLARA Clustering K=2")

ggsave("plots/claraout.jpg",claraout1)
ggsave("plots/clara2clusters.jpg",claraout2)
ggsave("plots/claraoutliers.jpg",
       multiplot(claraout1,claraout2,cols=2))

##clustering from k=1 to k=6
claraclustersgraph<-ggplot(data=dataclara,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("CLARA Clustering")

ggsave("plots/claraclusters.jpg",claraclustersgraph)

##densities from k=1 to k=6
ggplot(data=dataclara,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#cluster k=2 and k=3
clara2<-dataclara %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("CLARA K=2")

clara3<-dataclara %>%
    filter(k=="K=3") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("CLARA K=3")

ggsave("plots/clarak2k3.jpg",multiplot(clara2,clara3,cols=2))


#densities for all variables k=2 and k=3
plotlistclara2<-dataclara %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/claradensityk2.jpg",
       multiplot(plotlist=plotlistclara2,cols=3))

plotlistclara3<-dataclara %>% 
    filter(k=="K=3") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/claradensityk3.jpg",
       multiplot(plotlist=plotlistclara3,cols=3))



####hierarchical clustering method=average####
##depends on the used distance matrix and the method for calculating
##cluster distance

#dendrogram
hclustavg<-hclust(distance,method="average")
jpeg("plots/dendrogramaverage.jpg")
plot(hclustavg,hang=-1,labels=FALSE)
dev.off()

##calculate vectors of clusters
avgclusters<-hcluster(distance,k,method="average")
##calculate silhouette
sil.avg<-sil.clust(avgclusters,distance,k[2:6])
silavg<-data.frame(k=k[2:6],silhouette=sil.avg)
ggsave("plots/avgsil.jpg",ggplot(data=silavg,aes(x=k,y=silhouette))+
           geom_line()+geom_point()+
           ggtitle("Silhouette Average Clustering"))

#preparing data for plotting

dataavg<-addclustdf(data = datapca,vectors = avgclusters,k = k,
                       colnames = colnames)

#summary of means by cluster
summaryavg<-dataavg %>%
    group_by_("k","cluster") %>%
    summarize_all(min) %>%
    left_join(dataavg %>%
                  group_by_("k","cluster") %>%
                  summarize(count=n()),by=c("k","cluster")) %>%
    select(1:2,20,3:19)



#plots
#plot of count
ggplot(data=summaryavg,aes(x=k,y=count,fill=cluster))+
    geom_col(position="dodge")

#k=2 vs outliers
avgout1<-dataavg %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=noutliers))+geom_point()+
    ggtitle("Outliers")

avgout2<-dataavg %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Average Clustering K=2")

ggsave("plots/avgout.jpg",avgout1)
ggsave("plots/avg2clusters.jpg",avgout2)
ggsave("plots/avgoutliers.jpg",multiplot(avgout1,avgout2,cols=2))

##clustering from k=1 to k=6
avgclustersgraph<-ggplot(data=dataavg,aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    facet_wrap(~k,ncol=3)+ggtitle("Average Clustering")

ggsave("plots/avgclusters.jpg",avgclustersgraph)

##densities from k=1 to k=6
ggplot(data=dataavg,aes(x=kurtip,color=cluster))+geom_density()+
    facet_wrap(~k,ncol=3)

#cluster k=2 and k=3
avg2<-dataavg %>%
    filter(k=="K=2") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Average K=2")

avg3<-dataavg %>%
    filter(k=="K=3") %>%
    ggplot(aes(x=PC1,y=PC2,color=cluster))+geom_point()+
    ggtitle("Average K=3")

ggsave("plots/avgk2k3.jpg",multiplot(avg2,avg3,cols=2))


#densities for all variables k=2 and k=3
plotlistavg2<-dataavg %>% 
    filter(k=="K=2") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/avgdensityk2.jpg",
       multiplot(plotlist=plotlistavg2,cols=3))

plotlistavg3<-dataavg %>% 
    filter(k=="K=3") %>% 
    select(c(1:8,18,19)) %>% 
    multiplehist2("cluster")

ggsave("plots/avgdensityk3.jpg",
       multiplot(plotlist=plotlistavg3,cols=3))

