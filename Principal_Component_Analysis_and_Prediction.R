library(dplyr)
library(magrittr)
library(ggplot2)
library(corrgram)
library(data.table)
library(randomForest)
library(e1071)
library(caret)

str(df_terror)
corr_new <- round(cor(df_terror[sapply(df_terror, is.numeric)], use="pair"),2)
class(corr_new)
colnames(corr_new)
corr_nkill <- subset(corr_new, select = colnames(corr_new) == "nkill")
corr_df<-data.frame(row=rownames(corr_nkill)[row(corr_nkill)], col=colnames(corr_nkill)[col(corr_nkill)], corr=c(corr_nkill))
corr_useful<- corr_df%>%mutate(cor_abs = abs(corr_df$corr)) %>% arrange(desc(cor_abs))
head(corr_useful,100)
plot(corr_useful$cor_abs, type="l")
sum(is.na(df_terror))

#princomp(formula = ~., data = df_terror[sapply(df_terror, is.numeric)], cor = TRUE, na.action=na.exclude)
str(df_terror[sapply(df_terror, is.numeric)])
keeps <- c("eventid", "iyear","imonth","iday","extended","country","region","latitude","longitude","specificity",
           "vicinity","crit1","crit2","crit3","doubtterr","success","suicide","attacktype1","targtype1",
           "targsubtype1","natlty1","ingroup","weaptype1","nkill","nkillus","nkillter","property","ishostkid","ransom",
           "INT_LOG","INT_IDEO","INT_MISC","INT_ANY","nwound","claimmode3")
df_num <-df_terror[sapply(df_terror, is.numeric)]
df<- df_num[keeps]
df[is.na(df)] <- 0
str(df)
pca<-prcomp(df, center = TRUE, scale = TRUE, na.action = na.omit)
plot(pca, type="l")
summary(pca)
spca = summary(pca)
plot(spca$importance[3,], type="l")
pca_df <- data.frame(pca$x)
names(pca_df)
pca_df <- pca_df %>% dplyr::select(PC1:PC25) 
pca_df$y = df$nkill

# Linear Regression with Poisson 
model <- lm(data = pca_df, y ~ ., family = poisson(link=log))
summary(model)

# Generalized Linear Model with Quassi Poisson Regression
model2 <- glm(data = df, nkill ~ nkillus + nkillter + nwound + claimmode3 + ishostkid, family = quasipoisson(link=log))
summary(model2)


# Grouping the kills into low medium and high and predicting the category
df_terror <- read.csv("C:\\DCU\\Data Mining\\Mini Projects\\globalterrorismdb.csv",stringsAsFactors = TRUE)
str(df_terror[sapply(df_terror, is.numeric)])
keeps <- c("eventid", "iyear","imonth","iday","extended","country","region","latitude","longitude","specificity",
           "vicinity","crit1","crit2","crit3","doubtterr","success","suicide","attacktype1","targtype1",
           "targsubtype1","natlty1","ingroup","weaptype1","nkill","nkillus","nkillter","property","ishostkid","ransom",
           "INT_LOG","INT_IDEO","INT_MISC","INT_ANY","nwound","claimmode3")
df_num <-df_terror[sapply(df_terror, is.numeric)]
df<- df_num[keeps]
df[is.na(df)] <- 0
# Dimensions
df_terror_new <- df %>% filter(nkill != 0)
dim(df_terror_new)
head(df_terror_new$nkill)
df_terror_new$kill <- ifelse(df_terror_new$nkill > 5, 1, 0)
head(df_terror_new$kill)
df_terror_new$kill <- as.factor(df_terror_new$kill)

table(df_terror_new$kill)/nrow(df_terror_new)

# Creating the Sample 
sample.ind <- sample(2, 
                     nrow(df_terror_new),
                     replace = T,
                     prob = c(0.6,0.4))
df_terror_new.train <- df_terror_new[sample.ind==1,]
df_terror_new.test <- df_terror_new[sample.ind==2,]

table(df_terror_new.train$kill)/nrow(df_terror_new.train)
table(df_terror_new.test$kill)/nrow(df_terror_new.test)

# Building The Model

cols <- names(df_terror_new.train)
# Exclude ID or Response variable
cols <- cols[!cols %in% c("kill","nkill")]

# add + sign between exploratory variables
cols1 <- paste(cols, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("kill", cols1, sep = " ~ "))
df_terror_new.train[is.na(df_terror_new.train)] <- 0
df_terror_new.rf <- randomForest(rf.form,
                              df_terror_new.train,
                              ntree=50,
                              importance=T)

# Error Rates
plot(df_terror_new.rf)
# Variable Importance
varImpPlot(df_terror_new.rf,
           sort = T,
           main="Variable Importance",
           n.var=5)
df_terror_new.train$predicted.response <- predict( df_terror_new.rf , df_terror_new.train)

# Create Confusion Matrix
confusionMatrix(df_terror_new.train$predicted.response,
                reference=df_terror_new.train$kill)

df_terror_new.test$predicted.response <- predict(df_terror_new.rf ,df_terror_new.test)

# Create Confusion Matrix
confusionMatrix(data=df_terror_new.test$predicted.response,
                reference=df_terror_new.test$kill)

