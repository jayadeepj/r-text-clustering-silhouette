
library(tm)
library(NMF)
library(ggplot2)
library(skmeans)


#----------------------------- Configurations ---------------------------------

setwd("C:/code/study/git/r-text-clustering-silhouette")
source("code/log_utils.R")

## The corpus data set name & path  has to be changed for multiple iterations

#dataset.name <- "P1 - Corpus"
#dataset.path <-  "data/syslogs/corpus_real_pair_1/system_log_labelled_dataset_corpus_1_120DP_20C.csv"


#dataset.name <- "P2 - Corpus"
#dataset.path <-  "data/syslogs/corpus_real_pair_2/system_log_labelled_dataset_corpus_2_160DP_20C.csv"

dataset.name <- "P3 - Corpus"
dataset.path <-  "data/syslogs/corpus_real_pair_3/system_log_labelled_dataset_corpus_3_170DP_20C.csv"

#----------------------------- Declarations ---------------------------------
model.performance <- data.frame(cluster.model=character(),dataset=character(),  iteration=numeric() ,entropy=numeric(),purity=numeric(), silhoutte.index=numeric(),stringsAsFactors = FALSE)
data.folds <- 11
iteration.count <- 10
number.of.clusters <- 20

#----------------------------- Read Data ---------------------------------


for (iteration.index in 1:iteration.count)
{
  
  system.logs.labeled.events <- read.delim(dataset.path,header =TRUE, sep=",", stringsAsFactors = FALSE)
  
  count.system.logs.labeled.events <- nrow(system.logs.labeled.events)
  
  for(i in 1:data.folds)
  {
    system.logs.labeled.events <- rbind(system.logs.labeled.events,system.logs.labeled.events)
  }
  
  system.logs.labeled.events <- system.logs.labeled.events[sample(rep(1:count.system.logs.labeled.events*data.folds)),]
  
  system.logs.labeled.events$CLASS <- as.factor(system.logs.labeled.events$CLASS)
  #----------------------------- Feature Clean Up ---------------------------------
  
  
  system.logs.events <- system.logs.labeled.events$LOG.EVENTS
  
  system.logs.events.dtm <- generate.dtm.function (text.data = system.logs.events)
  system.logs.events.dtm.matrix <- as.matrix(system.logs.events.dtm)
  
  system.logs.events.tf.idf <- generate.tf.idf.function(text.data.dtm = system.logs.events.dtm)
  system.logs.events.tf.idf.matrix <- as.matrix(system.logs.events.tf.idf)
  
  
  #-- ------------------------Kmeans Clustering ---------------------
  system.logs.kmeans.model <- NULL
  system.logs.kmeans.model <- kmeans(system.logs.events.tf.idf.matrix,centers = number.of.clusters)
  predicted.kmeans.class <- system.logs.kmeans.model$cluster
  
  system.logs.labeled.events <- cbind(system.logs.labeled.events,predicted.kmeans.class)
  system.logs.labeled.events$predicted.kmeans.class <- as.factor(system.logs.labeled.events$predicted.kmeans.class)
  
  # Cluster Evaluation 
  # Entropy -- Low Entropy better cluster
  entropy <- entropy(x = system.logs.labeled.events$predicted.kmeans.class,y = system.logs.labeled.events$CLASS)
  # Purity -- High Purity better cluster
  purity <- purity(x = system.logs.labeled.events$predicted.kmeans.class,y = system.logs.labeled.events$CLASS)
  
  #silhoutte index
  system.logs.events.tf.idf.dissimilarity.matrix <- dist(system.logs.events.tf.idf.matrix) #-> large (!)  
  silhouette.object <- silhouette(system.logs.kmeans.model$cluster, system.logs.events.tf.idf.dissimilarity.matrix)
  silhouette.object.summary <- summary(silhouette.object)
  silhouette.index <- silhouette.object.summary$avg.width
  
  model.performance <- rbind(model.performance,data.frame(model="K Means",dataset = dataset.name, iteration = iteration.index, entropy=entropy, purity=purity, silhouette.index = silhouette.index))
  
  #--------------------------S Kmeans - Cosine Similarity -----------------------
  system.logs.skmeans.model <- NULL
  purity <- 0
  entropy <- 0
  silhouette.index <-0
  system.logs.skmeans.model <- skmeans(system.logs.events.tf.idf.matrix,k = number.of.clusters)
  predicted.skmeans.class <- system.logs.skmeans.model$cluster
  
  system.logs.labeled.events <- cbind(system.logs.labeled.events,predicted.skmeans.class)
  system.logs.labeled.events$predicted.skmeans.class <- as.factor(system.logs.labeled.events$predicted.skmeans.class)
  
  # Cluster Evaluation 
  # Entropy -- Low Entropy better cluster
  entropy <- entropy(x = system.logs.labeled.events$predicted.skmeans.class,y = system.logs.labeled.events$CLASS)
  # Purity -- High Purity better cluster
  
  purity <- purity(x = system.logs.labeled.events$predicted.skmeans.class,y = system.logs.labeled.events$CLASS)
  
  #silhoutte index
  system.logs.events.tf.idf.dissimilarity.matrix <- dist(system.logs.events.tf.idf.matrix) #-> large (!)  
  silhouette.object <- silhouette(predicted.skmeans.class, system.logs.events.tf.idf.dissimilarity.matrix)
  silhouette.object.summary <- summary(silhouette.object)
  silhouette.index <- silhouette.object.summary$avg.width
  
  model.performance <- rbind(model.performance,data.frame(model="Sp K Means",dataset = dataset.name,iteration = iteration.index,entropy=entropy, purity=purity, silhouette.index = silhouette.index))
  system.logs.labeled.events <- system.logs.labeled.events[,c(1,3,4,2)]
}

model.performance$dataset <- as.factor( model.performance$dataset)


ggplot.relation.object <- ggplot(data = model.performance,aes(x = model.performance$iteration, y = model.performance$entropy, group = model.performance$model, color = model.performance$model))
ggplot.relation.object <-ggplot.relation.object+geom_point()+geom_line()+facet_wrap(~ dataset)+scale_color_manual(name = "model", values=c("Sp K Means"="red","K Means"="blue") )
ggplot.relation.object <-ggplot.relation.object+ ggtitle("Entropy vs Cluster Model")+xlab("Iterations")+ylab("Entropy Value")
ggsave(filename ="plots/syslog/entropy.png" , width=8, height=3.5, plot=ggplot.relation.object)
ggplot.relation.object

ggplot.relation.object <- ggplot(data = model.performance,aes(x = model.performance$iteration, y = model.performance$purity, group = model.performance$model, color = model.performance$model))
ggplot.relation.object <-ggplot.relation.object+geom_point()+geom_line()+facet_wrap(~ dataset)+scale_color_manual(name = "model", values=c("Sp K Means"="red","K Means"="blue") )
ggplot.relation.object <-ggplot.relation.object+ ggtitle("Purity vs Cluster Model")+xlab("Iterations")+ylab("Purity Value")
ggsave(filename ="plots/syslog/purity.png" , width=8, height=3.5,plot=ggplot.relation.object)
ggplot.relation.object

ggplot.relation.object <- ggplot(data = model.performance,aes(x = model.performance$iteration, y = model.performance$silhouette.index, group = model.performance$model, color = model.performance$model))
ggplot.relation.object <-ggplot.relation.object+geom_point()+geom_line()+facet_wrap(~ dataset)+scale_color_manual(name = "model", values=c("Sp K Means"="red","K Means"="blue") )
ggplot.relation.object <-ggplot.relation.object+ ggtitle("Silhouette Index  vs Cluster Model")+xlab("Iterations")+ylab("Silhouette Index ")
ggsave(filename ="plots/syslog/silhouette.png" ,width=8, height=3.5, plot=ggplot.relation.object)
ggplot.relation.object
