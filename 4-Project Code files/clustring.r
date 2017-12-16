
library(dplyr)
library(ISLR) 
# for gower similarity and pam
library(cluster) 
library(Rtsne) 
# for visualization
library(ggplot2) 
library(cluster)
dataa<-read.csv("creditdata.csv") 


#issimilarity matrix calculation
dataa$IsTerrorist=NULL
dataa$Field.of.education = NULL
dataa$X =NULL
dataa$Occupational.category = NULL
dataa$Nature.of.offense = NULL
dataa$Occupational.category = NULL
dataa$Status.of.case = NULL
dataa$Name = NULL


head(dataa)
daisy.mat <- as.matrix(daisy(dataa, metric="gower"))

#Clustering by pam algorithm

my.cluster <- pam(daisy.mat, k=2, diss = T)

#Cluster plot

clusplot(daisy.mat, diss = T, my.cluster$clustering, color = T, main="Clustring")
table(data$IsTerrorist)
table(my.cluster$clustering)
table(my.cluster$clustering,data$IsTerrorist )

yy <- c("cluster1","cluster2")
my.cluster$silinfo$widths[,3]
########################################Rock

install.packages("cba")
library(cba)

#datadeclus1
cluster.res <-rockCluster(as.matrix(datadeclus1), 12 )
cluster.output <- cbind(datadeclus1,cluster.res$cl)
#write.csv(cluster.output, file = "Rock clusters.csv", row.names = TRUE)
table(cluster.output$`cluster.res$cl`)
