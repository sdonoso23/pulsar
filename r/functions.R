library(tidyverse)


###function to generate list of histograms
multiplehist<-function(data){
    numeric<-map_lgl(data,is.numeric)
    newdata<-data[,numeric]
    aux<- function(data,column){
        a<-ggplot(data=data,mapping=aes_string(x=column))+geom_density()+labs(x=column)
        return(a)
    }
    colnames(newdata)
    lista<-map(colnames(newdata),~aux(newdata,.))
    return(lista)
}

multiplehist2<-function(data,color){
    numeric<-map_lgl(data,is.numeric)
    newdata<-data[,numeric]
    aux<- function(data,column,color){
        a<-ggplot(data=data,mapping=aes_string(x=column,color=color))+geom_density()+labs(x=column)
        return(a)
    }
    colnames(newdata)
    lista<-map(colnames(newdata),~aux(data,.,color))
    return(lista)
}





###function to plot multiple ggplots together, found on R Cookbook
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

#extract sum of squares off k means
kmeans.ss<-function(data,k){
    aux<-function(data,k){
        set.seed(1234)
        ss<-kmeans(data,nstart = 30,centers = k)$tot.withinss
        return(ss)
    }
    return(map_dbl(k,~aux(data,.)))        
}


#extract vector of clusters off k means
kmeans.clust<-function(data,k){
    aux<-function(data,k){
        set.seed(1234)
        vec<-kmeans(data,nstart = 30,centers = k)$cluster
        return(vec)
    }
    return(as.data.frame(map(k,~aux(data,.))))        
}

kmeans.results<-function(data,k){
    aux<-function(data,k){
        set.seed(1234)
        vec<-kmeans(data,nstart = 30,centers = k)
        
        return(list(vec$tot.withinss,vec$cluster))
    }
    return(map(k,~aux(data,.)))      
}


#extract vector of clusters off hclust
hclusters<-function(dmatrix,k,method){
    clust<-hclust(dmatrix,method=method)
    aux<-function(hclust,k){
        return(cutree(hclust,k))
    }
    return(as.data.frame(map(k,~aux(clust,.))))
}

#create df with clusters

addclustdf<-function(data,vectors,k,colnames){
    names(vectors)<-colnames
    
    dataclust<-data %>%
        bind_cols(vectors) %>%
        gather_(key="k",value="cluster",gather_cols = colnames)
    
    dataclust$cluster<-factor(dataclust$cluster,levels=k)
    dataclust$k<-factor(dataclust$k,levels=colnames,labels=paste("K=",k,sep=""))
    
    return(dataclust)
    
}

##calculate silhouette for a number of clusters

sil.clust<-function(clusters,distance,k){
    aux<-function(cluster,distance,k){
        sil<-silhouette(cluster[[k]],distance)
        return(mean(sil[,3]))
    }
    return(map_dbl(k,~aux(clusters,distance,.)))
}

pam.clusters<-function(data,k){
    aux<-function(data,k){
        set.seed(1234)
        vec<-pam(data,k=k)
        
        return(vec$clustering)
    }
    return(map(k,~aux(data,.)))      
}

