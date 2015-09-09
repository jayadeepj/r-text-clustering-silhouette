# r-text-clustering-silhouette
##Silhouette Threshold Based Text Clustering for Log Analysis


This repository contains the code implementation I used to write the below specified paper published in International Journal of Data Mining Techniques and Applications 

Silhouette Threshold Based Text Clustering for Log Analysis
International Journal of Data Mining Techniques and Applications, 
ISSN: 2278-2419  Volume: 04, Issue: 01, June 2015
http://www.ijdmta.com/papers/vol4issue1/JUN_15_IJDMTA_007.pdf 


## Code Example

TODO:

## Motivation -- Abstract of the paper

Automated Real-Time log analysis has been a dominant subject area of interest to both industry and academics. The heterogeneous nature of system logs, the disparate sources of logs (Infrastructure,Networks, Databases and Applications) and their underlying structure & formats makes the challenge harder. In this paper I present the less frequently used document clustering techniques to dynamically organize real time log events (e.g. Errors, warnings) to specific categories that are pre-built from a corpus of log archives. This kind of syntactic log categorization can be exploited for automatic log monitoring,priority flagging and dynamic solution recommendation systems. I propose practical strategies to cluster and correlate high volume log archives and high velocity real time log events; both in terms of solution quality and computational efficiency. First I compare two traditional partitional document clustering approaches to categorize high dimensional log corpus. In order to select a suitable model for our problem,Entropy, Purity and Silhouette Index are used to evaluate these different learning approaches. Then I propose computationally efficient approaches to generate vector space model for the real time log events. Then to dynamically relate them to the categories from the corpus, I suggest the use of a combination of critical distance measure and least distance approach. In addition, I introduce and evaluate three different critical distance measure

## Installation

The datasets can be found in 'data' folder. 

I used 3 separate log data sets pairs (for the corpus and real time data). The log events typically included Overflow alerts from messaging queues, Database adapter connection failures, Application server cluster failover warnings and SOAP web-service failure messages. Each of the real time data sets contain 10 new categories and 10 old categories that exist in the Corpus.
I used 2 Corpus - Real time pairs with log event counts 13200-20, 17600-20 & 18700-20. The number of words in the the pairs were 189640-327, 323950-323, 365530-370. All three corpus set had 20 categories and each real data set contained 20 old and 20 new categories. The automatic log data was fetched from the systems/servers. Being from disparate sources there was no unity in the set of original log attributes. 

However, in general the log data contained (not restricted to) the following original log attributes

 Time stamp of the Log Event
 Source of the log event, such as the class, function, or filename
 Log Category Information. For example, the severity - INFO, WARN, ERROR & DEBUG
 Script/Process names
 Brief Summary of the Log Event
 Detailed Description of Log Event/Error

Description/Event message
Out of these log attributes only the last attribute i.e Detailed Description of Log Event/Error Description/Event message was used for this implementation

Following R packages are used.

skmeans: Spherical k-Means Clustering
https://cran.r-project.org/web/packages/skmeans/index.html

tm: Text Mining Package
https://cran.r-project.org/web/packages/tm/index.html


NMF: Algorithms and Framework for Nonnegative Matrix Factorization (NMF)
https://cran.r-project.org/web/packages/NMF/index.html

ggplot2: An Implementation of the Grammar of Graphics
https://cran.r-project.org/web/packages/ggplot2/index.html

proxy: Distance and Similarity Measures
https://cran.r-project.org/web/packages/proxy/index.html

## API Reference

Following scripts need to be run separately

corpus_cluster_evaluation.R -- This script uses Simple K-Means & Spherical K-Means clustering approaches to categorize the log corpus. In order to select a suitable model for our problem,Entropy, Purity and Silhouette Index are used to evaluate these different clustering approaches. Plots with the comparison is generated in the 'plots' folder

produces --> entropy.png, purity.png, silhouette.png displaying the evaluation parameters of the chosen corpus

real_time_clustering_with_euclidean_measure.R -- This script does the following steps using Euclidean distance measure

Step 1: For each incoming real time log event apply the pre-process step as described in Section 3.1 of the Paper to clean and filter the data 
Step 2: Generate a TF-IDF matrix for the real time log event based on the pre-built TF-IDF matrix of the log corpus.. Details in section 7.2 of the Paper
Step 3: Adapt the centroid generated while clustering the log corpus to suit the TF-IDF vector of the real time log event.  Details in section 7.3 of the Paper
Step 4: The centroid (From the adapter K centroid set) with the lowest dissimilarity to the real time event is chosen as its closest centroid. 
Step 5: If distance from the closest centroid to the real time Event is greater than the chosen Critical Distance Measure (RMWSS, Cluster Radius, Silhouette Threshold), then classify the real time event as new a new category (unobserved in the corpus).
Step 6: Else the cluster with the closest centroid is chosen as the category of the real time log event

To evaluate the different critical distance measures, the accuracy for real time events classified as old or new (similar events observed or unobserved in the original corpus) are calculated. Similarly the accuracy scores are calculated for real time events classified correctly into its true class from original corpus.

produces --> threshold_accuracy_euclidean.pdf/png which displays Accuracy for different Critical Distance Measures  (for RMWSS, Cluster Radius, Silhouette Threshold)  for Euclidean Distance
		
real_time_clustering_with_cosine_measure.R -- This script does the following steps using Cosine distance measure

Step 1: For each incoming real time log event apply the pre-process step as described in Section 3.1 of the Paper to clean and filter the data 
Step 2: Generate a TF-IDF matrix for the real time log event based on the pre-built TF-IDF matrix of the log corpus.. Details in section 7.2 of the Paper
Step 3: Adapt the centroid generated while clustering the log corpus to suit the TF-IDF vector of the real time log event.  Details in section 7.3 of the Paper
Step 4: The centroid (From the adapter K centroid set) with the lowest dissimilarity to the real time event is chosen as its closest centroid. 
Step 5: If distance from the closest centroid to the real time Event is greater than the chosen Critical Distance Measure (RMWSS, Cluster Radius, Silhouette Threshold), then classify the real time event as new a new category (unobserved in the corpus).
Step 6: Else the cluster with the closest centroid is chosen as the category of the real time log event

To evaluate the different critical distance measures, the accuracy for real time events classified as old or new (similar events observed or unobserved in the original corpus) are calculated. Similarly the accuracy scores are calculated for real time events classified correctly into its true class from original corpus.

produces --> threshold_accuracy_sphkmeans.pdf/png which displays Accuracy for different Critical Distance Measures (for RMWSS, Cluster Radius, Silhouette Threshold)  for Cosine Distance
		

## Plots

Some of the data visualizations are saved as pdfs/pngs in plots folder. 

categorization_accuracy.png -- CLuster prediction accuracy for Cosine & Euclidean distance measures
entropy.png -- Entropy values for various iteration for both K-Means & Spherical-K-Means
purity.png -- Purity values for various iteration for both K-Means & Spherical-K-Means
silhouette.png -- Silhouette Threshold values for various iteration for both K-Means & Spherical-K-Means
threshold_accuracy_euclidean.png -- which displays Accuracy for different Critical Distance Measures  (for RMWSS, Cluster Radius, Silhouette Threshold)  for Euclidean Distance
threshold_accuracy_sphkmeans.png -- displays Accuracy for different Critical Distance Measures (for RMWSS, Cluster Radius, Silhouette Threshold)  for Cosine Distance
threshold_rmse_euclidean.pdf -- RMSE for various Critical Distance Measures − Euclidean Distance
threshold_rmse_sphkmeans.pdf -- RMSE for various Critical Distance Measures − Cosine Distance

## Contributors

@jayadeepj

## License

:)