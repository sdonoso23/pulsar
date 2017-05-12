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

map_dbl(c(1:10),~aux(dataset,.))


#extract vector of clusters off k means
kmeans.clust<-function(data,k){
    aux<-function(data,k){
        set.seed(1234)
        vec<-kmeans(data,nstart = 30,centers = k)$cluster
        return(vec)
    }
    return(as.data.frame(map(k,~aux(data,.))))        
}

#extract vector of clusters off hclust
clustvectors<-function(dmatrix,k,method){
    clust<-hclust(dmatrix,method=method)
    aux<-function(hclust,k){
        return(cutree(hclust,k))
    }
    return(as.data.frame(map(k,~aux(clust,.))))
}

