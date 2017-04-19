#Prediction Model
#The goal is to group the terrorist groups based on the regions
#This is an unsupervised learning
#K-Means clustering - (Parition)
#Target - Gname (terrorist groups) 

#Used Features
features_used <- c('gname','iyear','region_txt','multiple','success','suicide', 'natlty1_txt')
#Relevant Fields
df_groups = df_terror[features_used]
head(df_groups)
str(df_groups)

#Top 10 Groups
list <- c('Taliban', 'Shining Path (SL)', 'Islamic State of Iraq and the Levant (ISIL)',
          'Farabundo Marti National Liberation Front (FMLN)',
          'Revolutionary Armed Forces of Colombia (FARC)', 'Basque Fatherland and Freedom (ETA)',
          'Irish Republican Army (IRA)', 'Boko Haram', 'Communist Party of India - Maoist (CPI-Maoist)')
df_groups <- df_groups %>% filter(gname %in% list)
str(df_groups)

#All numeric fields
df_num <-df_groups[sapply(df_groups, is.numeric)]
str(df_num)
df_num$region_txt <- df_groups$region_txt

#Replace NA by 0 in the numeric fields
df_num[is.na(df_num)] <- 0
str(df_num)

#Remove fields with na in the categorical 
df_num <- df_num[complete.cases(df_num),]
dim(df_num)
str(df_num)

#Converting character to factor
df_num$region_txt <- as.factor(df_num$region_txt)
#str(df_groups)

#Converting Factor to numerical
df_num$region_txt <- as.numeric(df_num$region_txt)
df_num$gname <- df_groups$gname
#df_num$gname <- as.numeric(df_num$gname)
str(df_num)

#   Use all the columns except the gname
vars.to.use <- colnames(protein)[-6]   

## optionally, store the centers and 
# standard deviations of the original data,
# so you can "unscale" it later.
pcenter <- attr(pmatrix, "scaled:center")  
pscale <- attr(pmatrix, "scaled:scale")

#Before running clusterboot() we’ll cluster the data using a hierarchical clustering algorithm (Ward’s method):

#   Create the distance matrix.
d <- dist(pmatrix, method="euclidean") 
print(d)
#   Do the clustering. 
pfit <- hclust(d, method="ward.D2") 
pfit

#   Plot the dendrogram.
plot(pfit, labels=df_num$gname) 
rect.hclust(pfit, k=5)

#   A convenience function 
print_clusters <- function(labels, k) {             
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

# get the cluster labels
groups <- cutree(pfit, k=5)

# load the fpc package
library(fpc)   

# set the desired number of clusters                               
kbest.p<-5       

#   Run clusterboot() with hclust 
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI,
                            method="ward", k=kbest.p)

#   The results of the clustering are in 
#   cboot.hclust$result. The output of the hclust() 
#   function is in cboot.hclust$result$result. 
#   cboot.hclust$result$partition returns a 
#   vector of clusterlabels. 
groups<-cboot.hclust$result$partition  


#The vector of cluster stabilities. 
# Values close to 1 indicate stable clusters
cboot.hclust$bootmean  

#Kmeans-clustering
k <-kmeans(df_num, centers=3)
plot(k)
print(k)

#Evaluate k_means
km.boot <- clusterboot(distxy, B=20, bootmethod="boot",
                       clustermethod=kmeansCBI,
                       krange=3, seed=1)

km.boot


