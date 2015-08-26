
library(tm)
library(NMF)
library(proxy)
library(ggplot2)

#----------------------------- Configurations -----------------------------------------------------

setwd("C:/code/study/git/r-text-clustering-silhouette")
source("code/log_utils.R")


#----------------------------- Declarations ---------------------------------------------------------

model.performance <- data.frame(cluster.model=character(),dataset=character(), iteration=numeric() ,entropy=numeric(),purity=numeric(),accuracy=numeric(),  stringsAsFactors = FALSE)
threshold.performance <- data.frame(cluster.model= character(), dataset=character(),iteration=numeric() ,threshold.type=character(), accuracy=numeric(), stringsAsFactors = FALSE)

set.seed(80)

iteration.index <-1
data.folds <- 5
iteration.count <- 10
corpus.event.length <- 120
real.time.event.length <- 20
number.of.clusters <- 20
#----------------------------- Read Data -------------------------------------------------------------

for (iteration.index in 1:iteration.count)
{
  ## The real-time data set name & path  has to be changed for multiple iterations
  
  #   dataset.name <- "P1 - Real Time"
  #   corpus.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_1/system_log_labelled_dataset_corpus_1_120DP_20C.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
  #   real.time.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_1/system_log_labelled_dataset_real_1_20DP.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
  #   
  #   dataset.name <- "P2 - Real Time"
  #   corpus.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_2/system_log_labelled_dataset_corpus_2_160DP_20C.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
  #   real.time.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_2/system_log_labelled_dataset_real_2_20DP.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
  #   
      dataset.name <- "P3 - Real Time"
      corpus.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_3/system_log_labelled_dataset_corpus_3_170DP_20C.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
      real.time.system.logs.labeled.events <- read.delim("data/syslogs/corpus_real_pair_3/system_log_labelled_dataset_real_3_20DP.csv",header =TRUE, sep=",", stringsAsFactors = FALSE)
  
  corpus.event.length <- nrow(corpus.system.logs.labeled.events) * data.folds
  for(i in 1:data.folds)
  {
    corpus.system.logs.labeled.events <- rbind(corpus.system.logs.labeled.events,corpus.system.logs.labeled.events)
  }
  
  corpus.system.logs.labeled.events <- corpus.system.logs.labeled.events[sample(rep(1:corpus.event.length)),]
  cluster.index.association <- NULL
  cluster.index.association<-corpus.system.logs.labeled.events[,'CLASS']
  corpus.system.logs.labeled.events$CLASS <- as.factor(corpus.system.logs.labeled.events$CLASS)
  
  real.time.system.logs.labeled.events$IS.OLD <- as.factor(real.time.system.logs.labeled.events$IS.OLD)
  real.time.system.logs.labeled.events <- real.time.system.logs.labeled.events[sample(rep(1:real.time.event.length)),]
  
  #----------------------------- Feature Clean Up - Corpus ---------------------------------
  
  corpus.system.logs.events <- corpus.system.logs.labeled.events$LOG.EVENTS
  
  corpus.system.logs.events.dtm <- generate.dtm.function (text.data = corpus.system.logs.events)
  corpus.system.logs.events.dtm.matrix <- as.matrix(corpus.system.logs.events.dtm)
  
  corpus.system.logs.events.tf.idf <- generate.tf.idf.function(text.data.dtm = corpus.system.logs.events.dtm)
  corpus.system.logs.events.tf.idf.matrix <- as.matrix(corpus.system.logs.events.tf.idf)
  
  #----------------------------- Feature Clean Up - Test ---------------------------------
  
  real.time.system.logs.events <- real.time.system.logs.labeled.events$LOG.EVENTS
  
  #this is a temp mechanism to get tf-idf of real.time data
  temp.combined.system.logs.events <- c(corpus.system.logs.events,real.time.system.logs.events)
  temp.combined.system.logs.events.dtm <- generate.dtm.function (text.data = temp.combined.system.logs.events)
  temp.combined.system.logs.events.dtm.matrix <- as.matrix(temp.combined.system.logs.events.dtm)
  
  temp.combined.system.logs.events.tf.idf <- generate.tf.idf.function(text.data.dtm = temp.combined.system.logs.events.dtm)
  
  real.time.system.logs.events.tf.idf <- temp.combined.system.logs.events.tf.idf[(corpus.event.length+1):length(temp.combined.system.logs.events),]
  real.time.system.logs.events.tf.idf.matrix <- as.matrix(real.time.system.logs.events.tf.idf)
  corpus.system.logs.events.tf.idf.matrix <- as.matrix(temp.combined.system.logs.events.tf.idf[1:corpus.event.length,])
  
  #-- ------------------------Kmeans Clustering -----------------------------------------------------------
  corpus.system.logs.kmeans.model <- NULL
  corpus.system.logs.kmeans.model <- kmeans(corpus.system.logs.events.tf.idf.matrix,centers = number.of.clusters)
  predicted.kmeans.class <- corpus.system.logs.kmeans.model$cluster
  
  corpus.system.logs.labeled.events <- cbind(corpus.system.logs.labeled.events,predicted.kmeans.class)
  
  cluster.index.association <- cbind(cluster.index.association,predicted.kmeans.class)
  corpus.system.logs.labeled.events$predicted.kmeans.class <- as.factor(corpus.system.logs.labeled.events$predicted.kmeans.class)
  corpus.system.logs.labeled.events <- corpus.system.logs.labeled.events[,c(1,3,2)]
  
  
  #------------------------Form best class association ----------------
  cluster.index.association.df <- as.data.frame(cluster.index.association)
  names(cluster.index.association.df) <- c('original.class','predicted.class')
  class.association <- data.frame(original.class=numeric() ,predicted.class=numeric())
  for(original.class.current in 1: number.of.clusters)
  {
    best.count <- -1
    best.predicted.class <- -1
    
    association.per.class <-cluster.index.association.df[with(cluster.index.association.df, cluster.index.association.df$original.class==original.class.current),]
    for(predicted.class.current in 1: number.of.clusters)
    {
      current.count <- nrow(association.per.class[with(association.per.class, association.per.class$predicted.class==predicted.class.current),])
      if (current.count > best.count)
      {
        best.count <- current.count
        best.predicted.class <- predicted.class.current 
      }
    }
    
    class.association <- rbind(class.association,data.frame(original.class=original.class.current,predicted.class=best.predicted.class ))
  }
  
  
  #-----------------------Modify the cluster centres to increase the width to include the new bow ---------------------------------------------------------
  
  # number.of.clusters X no of words of corpus dataset  (20 * 223)
  dim(corpus.system.logs.kmeans.model$centers)
  # real.time.event.length X no of words of combined dataset  (20 * 293)
  dim(real.time.system.logs.events.tf.idf.matrix)
  
  mod.system.logs.kmeans.model.centers <- as.matrix(corpus.system.logs.kmeans.model$centers)
  
  newly.added.bow.size <- ncol(real.time.system.logs.events.tf.idf.matrix) - ncol(corpus.system.logs.events.tf.idf.matrix)
  temp.matrix <- matrix(nrow = number.of.clusters, ncol = newly.added.bow.size )
  temp.matrix[,] <- 0
  
  mod.system.logs.kmeans.model.centers  <- cbind(mod.system.logs.kmeans.model.centers,temp.matrix)
  
  #------------------------------ Calculate cluster radis for each cluster ----
  cluster.radius.vector <- vector(mode = 'numeric', length = number.of.clusters )
  
  for(centre.label in 1:number.of.clusters)
  {
    
    corpus.system.logs.events.tf.idf.matrix.per.centre <- corpus.system.logs.events.tf.idf.matrix[corpus.system.logs.kmeans.model$cluster==centre.label,]
    if(is.null(nrow(corpus.system.logs.events.tf.idf.matrix.per.centre)))
    {
      corpus.system.logs.events.tf.idf.matrix.per.centre <- t(as.matrix(corpus.system.logs.events.tf.idf.matrix.per.centre))
    }
    corpus.system.logs.events.per.centre.distance.matrix<- proxy::dist(x = corpus.system.logs.events.tf.idf.matrix.per.centre, y = t(as.matrix(mod.system.logs.kmeans.model.centers[centre.label,])))
    cluster.radius.vector[centre.label] <- max(corpus.system.logs.events.per.centre.distance.matrix)
  }
  
  #------------------------------ Calculate Silhouette index for each cluster -------------
  cluster.silhouette.width.vector <- vector(mode = 'numeric', length = number.of.clusters )
  corpus.system.logs.dissimilarity.matrix <- dist(corpus.system.logs.events.tf.idf.matrix) #-> large (!)  
  silhouette.object <- silhouette(corpus.system.logs.kmeans.model$cluster, corpus.system.logs.dissimilarity.matrix)
  silhouette.object.summary <- summary(silhouette.object)
  cluster.silhouette.width.vector <- silhouette.object.summary$clus.avg.widths
  #--------------------------------------------------------------------------------------------
  
  # Calculate distance
  real.time.to.model.centers.distance.matrix <- proxy::dist(x = real.time.system.logs.events.tf.idf.matrix, y = mod.system.logs.kmeans.model.centers)
  
  dist.to.closest.centroid <- vector(mode = 'numeric', length = real.time.event.length )
  closest.centroid <- vector(mode = 'numeric', length = real.time.event.length )
  point.withinss.vector <- vector(mode = 'numeric', length = real.time.event.length )
  point.root.avg.withinss.vector <- vector(mode = 'numeric', length = real.time.event.length )
  point.radius.vector <- vector(mode = 'numeric', length = real.time.event.length )
  point.silhouette.width.vector <- vector(mode = 'numeric', length = real.time.event.length )
  point.class.to.be <- vector(mode = 'numeric', length = real.time.event.length )
  
  for(i in 1:real.time.event.length)
  {
    closest.centroid[i] <- order(real.time.to.model.centers.distance.matrix[i,],decreasing = FALSE)[1]
    dist.to.closest.centroid[i] <- real.time.to.model.centers.distance.matrix[i,closest.centroid[i]]
    point.withinss.vector[i]<- corpus.system.logs.kmeans.model$withinss[closest.centroid[i]]
    point.root.avg.withinss.vector[i] <- sqrt(corpus.system.logs.kmeans.model$withinss[closest.centroid[i]]/corpus.system.logs.kmeans.model$size[closest.centroid[i]])
    point.radius.vector[i] <- cluster.radius.vector[closest.centroid[i]] 
    point.silhouette.width.vector[i] <- cluster.silhouette.width.vector[closest.centroid[i]]
    
    if(!is.na(real.time.system.logs.labeled.events$CLASS[i])){
      point.class.to.be[i] <-  class.association$predicted.class[with(class.association,class.association$original.class== real.time.system.logs.labeled.events$CLASS[i])]
    }
  }
  
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,closest.centroid)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,dist.to.closest.centroid)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,point.withinss.vector)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,point.root.avg.withinss.vector)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,point.radius.vector)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,point.silhouette.width.vector)
  real.time.system.logs.labeled.events <- cbind(real.time.system.logs.labeled.events,point.class.to.be)
  
  real.time.system.logs.labeled.events$is.old.withinss <- ifelse(real.time.system.logs.labeled.events$dist.to.closest.centroid > real.time.system.logs.labeled.events$point.root.avg.withinss.vector,1,0)
  real.time.system.logs.labeled.events$is.old.radius <- ifelse(real.time.system.logs.labeled.events$dist.to.closest.centroid > (real.time.system.logs.labeled.events$point.radius.vector),1,0)
  real.time.system.logs.labeled.events$is.old.silhoutte <- ifelse(real.time.system.logs.labeled.events$dist.to.closest.centroid > ((1+real.time.system.logs.labeled.events$point.silhouette.width.vector)*real.time.system.logs.labeled.events$point.radius.vector),1,0)
  
  real.time.system.logs.labeled.events$is.old.withinss <-as.factor(real.time.system.logs.labeled.events$is.old.withinss)
  real.time.system.logs.labeled.events$is.old.radius <-as.factor(real.time.system.logs.labeled.events$is.old.radius)
  real.time.system.logs.labeled.events$is.old.silhoutte <-as.factor(real.time.system.logs.labeled.events$is.old.silhoutte)
  
  real.time.system.logs.labeled.events <- real.time.system.logs.labeled.events[,c(1,2,4,5,6,7,8,9,10,11,12,13,3)]
  
  # ---------------------------------Cluster Evaluation  Entropy/Purity---------------------------------------
  
  # Corpus Clustering evaluation 
  # Entropy -- Low Entropy better cluster
  corpus.entropy <- NMF::entropy(x = corpus.system.logs.labeled.events$predicted.kmeans.class,y = corpus.system.logs.labeled.events$CLASS)
  # Purity -- High Purity better cluster
  corpus.purity <- purity(x = corpus.system.logs.labeled.events$predicted.kmeans.class,y = corpus.system.logs.labeled.events$CLASS)
  #model.performance <- rbind(model.performance,data.frame(cluster.model="K Means - Euclidean", iteration = iteration.index, dataset = "corpus140" , entropy=corpus.entropy, purity=corpus.purity, accuracy =0))
  
  
  # Real Time Data Clustering evaluation
  combined.class <- real.time.system.logs.labeled.events$CLASS[!is.na(real.time.system.logs.labeled.events$CLASS)]
  combined.class <- rbind(combined.class,corpus.system.logs.labeled.events$CLASS )
  combined.class <- as.factor(combined.class)
  
  combined.predicted.kmeans.class <- real.time.system.logs.labeled.events$closest.centroid[!is.na(real.time.system.logs.labeled.events$CLASS)]
  combined.predicted.kmeans.class <- rbind(combined.predicted.kmeans.class,corpus.system.logs.labeled.events$predicted.kmeans.class )
  combined.predicted.kmeans.class <- as.factor(combined.predicted.kmeans.class)
  
  combined.entropy <-  NMF::entropy(x = combined.predicted.kmeans.class,y = combined.class)
  combined.purity <- purity(x = combined.predicted.kmeans.class,y = combined.class)
  
  
  
  
  #---------------------------- Real Time Data Clustering Evaluation Accuracy ---------------------------
  is.old.withinss.accuracy <- mean(with(data = real.time.system.logs.labeled.events,real.time.system.logs.labeled.events$IS.OLD==real.time.system.logs.labeled.events$is.old.withinss))
  is.old.radius.accuracy <- mean(with(data = real.time.system.logs.labeled.events,real.time.system.logs.labeled.events$IS.OLD==real.time.system.logs.labeled.events$is.old.radius))
  is.old.silhoutte.accuracy <- mean(with(data = real.time.system.logs.labeled.events,real.time.system.logs.labeled.events$IS.OLD==real.time.system.logs.labeled.events$is.old.silhoutte))
  
  
  threshold.performance <- rbind(threshold.performance,data.frame(cluster.model="K Means - Euclidean", threshold.type="RMWSS", iteration = iteration.index, dataset = dataset.name, accuracy=is.old.withinss.accuracy))
  threshold.performance <- rbind(threshold.performance,data.frame(cluster.model="K Means - Euclidean", threshold.type="Cluster Radius", iteration = iteration.index, dataset = dataset.name, accuracy=is.old.radius.accuracy))
  threshold.performance <- rbind(threshold.performance,data.frame(cluster.model="K Means - Euclidean", threshold.type="Silhouette Threshold", iteration = iteration.index, dataset = dataset.name, accuracy=is.old.silhoutte.accuracy))
  
}


threshold.performance$dataset <- as.factor( threshold.performance$dataset)
threshold.performance$threshold.type <- as.factor( threshold.performance$threshold.type)

ggplot.relation.object <- ggplot(data = threshold.performance,aes(x = threshold.performance$iteration, y = threshold.performance$accuracy, group = threshold.performance$threshold.type, color = threshold.performance$threshold.type))
ggplot.relation.object <-ggplot.relation.object+geom_point()+geom_line()+facet_wrap(~ dataset)+scale_color_manual(name = "Critical Distance Measure", values=c("RMWSS"="red","Cluster Radius"="blue","Silhouette Threshold"="green" ))
ggplot.relation.object <-ggplot.relation.object+ ggtitle("Critical Distance Measure Comparison - Euclidean Distance")+xlab("Iterations")+ylab("Accuracy")
ggsave(filename ="plots/syslog/threshold_accuracy_euclidean.pdf" , width=8, height=3.5, plot=ggplot.relation.object)
ggplot.relation.object

write.table(x = model.performance, file ="data/syslogs/Euclidean_Prediction.csv", sep="," ,append = FALSE,quote = FALSE, row.names = FALSE)

#---------------------------------------------------------------------------------------------------------

sorted.corpus.system.logs.labeled.events <- corpus.system.logs.labeled.events[order(corpus.system.logs.labeled.events$predicted.kmeans.class),]
sorted.real.time.system.logs.labeled.events <- real.time.system.logs.labeled.events[order(real.time.system.logs.labeled.events$dist.to.closest.centroid),]
