library(dplyr)
library(ggplot2)
library(randomForest)

# Predicting the success of a terror attack based on the available features

# This is a Supervised Classification problem as we have the target label of success/failure

head(df_terror)

# Pick only the useful features 
useful_features <- c('iyear','imonth','iday','country','region','latitude','longitude','specificity',
                     'vicinity','crit1','crit2','crit3','doubtterr','multiple','success','suicide',
                     'attacktype1','targtype1','targsubtype1','ingroup','guncertain1','weaptype1')
# Create a subset of the useful features 

df_clean <- df_terror[useful_features]
names(df_clean)
df_success <- df_clean %>% filter(success == 1)
df_success <- df_success[complete.cases(df_success),]

df_failure <- df_clean %>% filter(success == 0)
df_failure <- df_failure[complete.cases(df_failure),]

dim(df_success)
dim(df_failure)
df_success <- sample_n(df_success,12477)
dim(df_success)

yf=df_failure %>% select(success)
#df_failure$success <- NULL

ys= df_success %>% select(success)
#df_success$success <- NULL
df <- rbind(df_success,df_failure)
dim(df)


# 10 Fold Cross validation 
# Shuffle
df<-df[sample(nrow(df)),]
# generate array containing fold-number for each sample (row)
folds <- cut(seq(1,nrow(df)),breaks=10,labels=FALSE)

for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df[testIndexes, ]
  trainData <- df[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}
Y_train <- trainData %>% select(success)
dim(Y_train)
y_test  <- testData %>% select(success)
trainData$success <- NULL
testData$success <- NULL

trainData$success <- as.character(trainData$success)
trainData$success <- as.factor(trainData$success)

testData$success <- as.character(testData$success)
testData$success <- as.factor(testData$success)
rf <- randomForest(success ~ .,trainData, ntree=100, importance=TRUE)
predicted <- predict(rf,testData)
print(predicted)
print(paste("Accuracy :",mean(predicted == testData$success)))
