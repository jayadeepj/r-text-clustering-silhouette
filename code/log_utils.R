#-------------------------------- FUNCTIONS -----------------------------------------------------


#----------------------------------- Function to Form Term Document Matrix ----------------------
generate.dtm.function <- function(text.data)
{
  
  text.data.vector.source <- VectorSource(x = text.data)
  text.data.vector.corpus <-VCorpus(x = text.data.vector.source)
  
  dtm.control.list <- list(removePunctuation=TRUE, removeNumbers=TRUE, stopwords=TRUE, minDocFreq=2)
  text.data.dtm <- DocumentTermMatrix(x = text.data.vector.corpus,control = dtm.control.list)
  
  return (text.data.dtm)
}

#----------------------------------- Function to Form IF IDF ----------------------
generate.tf.idf.function <- function(text.data.dtm)
{
  
  text.data.tf.idf <- weightTfIdf(m = text.data.dtm)
  return (text.data.tf.idf)
}

calculate.cluster.purity.by.formula <- function(labelled.classes,predicted.clusters)
{
  sum(apply(table(labelled.classes, predicted.clusters), 2, max)) / length(predicted.clusters)
}

#---------------------------- Generate Cluster visualization ---------------------------------

generate.cluster.visualization <- function(system.logs.events.tf.idf.matrix, system.logs.labeled.events)
{
  system.logs.visualization.data <- system.logs.events.tf.idf.matrix[,1:ncol(system.logs.events.tf.idf.matrix)]
  system.logs.mds.fit <- cmdscale(d = dist(system.logs.visualization.data), k = 2)
  
  system.logs.mds.fit.df <-data.frame(x = system.logs.mds.fit[,1],y = system.logs.mds.fit[,2])
  system.logs.mds.fit.df$predicted.kmeans.class <- as.factor(system.logs.labeled.events$predicted.kmeans.class)
  
  ggplot.relation.object <- ggplot(system.logs.mds.fit.df, aes(x=system.logs.mds.fit.df$x,y=system.logs.mds.fit.df$y, color = system.logs.mds.fit.df$predicted.kmeans.class))
  ggplot.relation.object <-ggplot.relation.object+geom_point(size=5, alpha=.3)
  ggplot.relation.object
}