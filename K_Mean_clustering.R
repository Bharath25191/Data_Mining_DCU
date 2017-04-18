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
#df_groups<- na.omit(df_groups$region_txt)
str(df_groups)

#All numeric fields
df_num <-df_groups[sapply(df_groups, is.numeric)]
str(df_num)
df_num$gname <- df_groups$gname
df_num$region_txt <- df_groups$region_txt

str(df_num)

#Replace NA by 0
df_num[is.na(df_num)] <- 0
str(df_num)

df_num <- df_num[complete.cases(df_num),]
dim(df_num)


#hot encoding
df_num <- df_num[df_num$gname, df_num$region_txt]
str(df_num)

#Use all columns except gname
#Converting character to factor
df_num$region_txt <- as.factor(df_num$region_txt)
#str(df_groups)

#Converting Factor to numerical
df_num$region_txt <- as.numeric(df_num$region_txt)
df_num$gname <- as.numeric(df_num$gname)
str(df_num)

#J
install.packages("flexclust")
library(flexclust)
data("Nclus")
df <- as.data.frame(Nclus)
str(df)
set.seed(1)
dat <- df_num
ind <- sample(nrow(dat), 50)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

cl1 = kcca(dat[dat[["train"]]==TRUE, 1:2], k=4, kccaFamily("kmeans"))
cl1    


pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 1:2])

image(cl1)
points(dat[dat[["train"]]==TRUE, 1:2], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 1:2], col=pred_test, pch=22, bg="orange")
#J

#Kmeans-clustering
k <-kmeans(df_num, centers=3)
print(k)

#Evaluate k_means
#install.packages('fpc')
library(fpc)
km.boot <- clusterboot(df_num, B=20, bootmethod="boot",
                       clustermethod=kmeansCBI,
                       krange=3, seed=1)

km.boot




