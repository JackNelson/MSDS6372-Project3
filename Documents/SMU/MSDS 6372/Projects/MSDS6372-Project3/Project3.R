#store current working directory for end of program
dir<-getwd()

#installing other heatmap packages
#install.packages('heatmap.plus')
#install.packages('pheatmap')

#load necessary packages needed for program
library(ggplot2)
library(heatmap.plus)
library(pheatmap)
library(gridExtra)
library(reshape2)
library(dendextend)




######### Reading in Data and Cleaning #########

#read in csv file and assign to dataframe breast_cancer
breast_cancer<-read.csv("/Users/JackNelson/Downloads/BreastCancer.txt",stringsAsFactors=FALSE, header=TRUE)

#initialize sample group variable
breast_cancer$samp_group <- 0
#initialize sample group cut points
temp <- c(367,70,31,17,48,49,31,86)

#assigning sample group value for each observation
for (i in 1:nrow(breast_cancer)) {
  k<-0
  for (j in 1:length(temp)) {
    k<-k+temp[j]
    if (breast_cancer$samp_group[i]==0) {
      if (i<=k) {
        breast_cancer$samp_group[i]<-j      
      }
    }
  }
}

#removing observations with missing values in bare_nuc
breast_cancer2<-breast_cancer[which(breast_cancer$bare_nuc != '?'),]
#convert bare_nuc variable to numeric
breast_cancer2$bare_nuc<-as.integer(breast_cancer2$bare_nuc)




######### Correlation Plot Heat Map #########

qplot(x=Var1, y=Var2, data=melt(cor(breast_cancer2[,2:10], use = "p")), 
      fill = value, geom = "tile") + 
  scale_fill_gradient2(limits = c(-1, 1)) + 
  geom_text(aes(label = round(value, 2))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(title = "Attribute Correlations")


######### Measuring Clustering Accuracy for Class #########

##### Looping through heirarchial methods for clustering

#assign working df
breast_cancer3 <- breast_cancer2
#assign number of columns in df
df.columns <- ncol(breast_cancer3)
#calculating distances between points
distance <- dist(breast_cancer2[,c(2:10)], method = "euclidean")
#list of heirarchial clusting methods to be used
hclust_methods <- c("ward.D", "single", "complete", "average", "centroid")
#initialize perc verctor
perc <- vector()

#looping through each heirarchial method
for (i in 1:length(hclust_methods)) {
  #assign clust vector to clustering results
  clust <- cutree(hclust(d=distance, method=hclust_methods[i]), k=2)
  #append results to df
  breast_cancer3[,(df.columns+i)] <- clust
  
  #initialize accuracy count
  count <- 0
  #looping to get number of cancerous lumps in each class
  for (j in 1:nrow(breast_cancer3)) {
    #test for accuracy
    if ((breast_cancer3$class[j]/2) == clust[j]) {
      count <- count+1
    }
    else {}
  }
  #inverting accuracy if lower than 0.5 (lowest possible value)
  if (count/nrow(breast_cancer3)<0.5) {
    perc[i] <- 1-(count/nrow(breast_cancer3))
  }
  else {
    perc[i] <- count/nrow(breast_cancer3)
  }
}
#assign colnames from heirarchial methods vector
colnames(breast_cancer3)[(df.columns+1):(df.columns+length(hclust_methods))] <- hclust_methods
#combine results with labels
clust_acc <- cbind(hclust_methods,perc)

##### Partitioning methods for clustering

#kmeans clustering 
kmeans.clust <- kmeans(breast_cancer2[,c(2:10)],centers=2)

#initialize accuracy count
count <- 0
#looping to get number of cancerous lumps in each class
for (j in 1:nrow(breast_cancer3)) {
  #test for accuracy
  if ((breast_cancer3$class[j]/2) == kmeans.clust$cluster[j]) {
    count <- count+1
  }
  else {}
}

#inverting accuracy if lower than 0.5 (lowest possible value)
if (count/nrow(breast_cancer3)<0.5) {
  kmeans.perc <- cbind("K.means", 1-(count/nrow(breast_cancer3)))
} else kmeans.perc <- cbind("K.means", count/nrow(breast_cancer3))

#assigning vector with all clustering methods accuracries
clust_acc <- rbind(clust_acc, kmeans.perc)

#assign clust_acc matrix to df for display
clust_acc_df <- data.frame(clust_acc)
colnames(clust_acc_df) <- c("Method", "Accuracy")

#generate percent with 2 decimal places
clust_acc_df$Accuracy <- paste(formatC(as.numeric(paste(clust_acc_df$Accuracy))*100), "%")

#display accuracy for each method in table
grid.table(clust_acc_df)



######### Heatmap & Dendragram #########

#heirarchial clustering
#taken from http://www.statmethods.net/advstats/cluster.html
fit <- hclust(distance, method="ward.D")
plot(fit)

#generating a dendrogram for heirarchial clustering
groups <- cutree(fit, k=2)
#marking number of clusters with red boxes
rect.hclust(fit,k=2,border="red")

#normalizing explanatory variables to uniform scale
breast_cancer.scaled <- as.matrix(scale(breast_cancer2[,c(2:10)]))

#creating list of colors for observations based on variable value
class1_cols <- c("red","blue")[(breast_cancer2$class)/2]
class2_cols <- c("green","yellow","orange","cyan","black","brown","purple","gray")[breast_cancer2$samp_group]
#putting lists in a data.frame to reference
anno <- data.frame(class=class1_cols,group=class2_cols)

dend1 <- as.dendrogram(fit)
cols_branches <- c("forestgreen", "orange")
dend1 <- color_branches(dend1, k = 2, col = cols_branches)

#heatmap with ribbon indicators for class and group 
heatmap.plus(as.matrix(scale(breast_cancer2[,c(2:10)])), 
             RowSideColors = as.matrix(anno), cexCol = .75, 
             Rowv = dend1, labRow = FALSE, ylab = "Observations")
legend(x=-100, y=3200, legend = c("Benign","Malignant","Sample","Sample 2","Sample 3", 
                              "Sample 4","Sample 5","Sample 6","Sample 7","Sample 8"), 
       fill = c("blue", "red", "green","yellow","orange","cyan","black","brown",
                "purple","gray"), 
       bty="n", border = FALSE, y.intersp = 0.7, cex = 0.7)

     