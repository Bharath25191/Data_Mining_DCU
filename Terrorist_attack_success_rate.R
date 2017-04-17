library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(e1071)
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
conf <- table(testData$success, predicted)
print(conf)
accuracy <- sum(diag(conf))/sum(conf)
print(accuracy)

cm <- confusionMatrix(data = predicted, reference = testData$success)

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(cm)
