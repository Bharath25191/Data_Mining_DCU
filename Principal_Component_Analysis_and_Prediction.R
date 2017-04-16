library(dplyr)
library(magrittr)
library(ggplot2)
library(corrgram)
str(df_terror)
# Selecting only the numerical features
#corr <-cor(df_terror[sapply(df_terror, is.numeric)], use="pairwise")
#corr_df<-data.frame(row=rownames(corr)[row(corr)], col=colnames(corr)[col(corr)], corr=c(corr))
#head(corr_df)
#corr_useful <-corr_df[corr_df$col == "nkill", ]
#head(corr_useful)
#corr_useful<- corr_useful%>%mutate(cor_abs = abs(corr_useful$corr)) %>% arrange(desc(cor_abs))
#head(corr_useful,100)
#plot(corr_useful$cor_abs, type="l")
corr_new <- round(cor(df_terror[sapply(df_terror, is.numeric)], use="pair"),2)
class(corr_new)
#corr_df<-data.frame(row=rownames(corr_new)[row(corr_new)], col=colnames(corr_new)[col(corr_new)], corr=c(corr_new))
#head(corr_df,10)
colnames(corr_new)
corr_nkill <- subset(corr_new, select = colnames(corr_new) == "nkill")
print(corr_nkill)
corr_df<-data.frame(row=rownames(corr_nkill)[row(corr_nkill)], col=colnames(corr_nkill)[col(corr_nkill)], corr=c(corr_nkill))
corr_useful<- corr_df%>%mutate(cor_abs = abs(corr_df$corr)) %>% arrange(desc(cor_abs))
head(corr_useful,100)
plot(corr_useful$cor_abs, type="l")
sum(is.na(df_terror))

princomp(formula = ~., data = df_terror[sapply(df_terror, is.numeric)], cor = TRUE, na.action=na.exclude)
str(df_terror[sapply(df_terror, is.numeric)])
keeps <- c("eventid", "iyear","imonth","iday","extended","country","region","latitude","longitude","specificity",
           "vicinity","crit1","crit2","crit3","doubtterr","success","suicide","attacktype1","targtype1",
           "targsubtype1","natlty1","ingroup","weaptype1","nkill","nkillus","nkillter","property","ishostkid","ransom",
           "INT_LOG","INT_IDEO","INT_MISC","INT_ANY")
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
pca_df <- pca_df %>% select(PC1:PC25) 
pca_df$y = df$nkill

model <- lm(data = pca_df, y ~ .)
summary(model)

model2 <- glm(data = pca_df, y ~ .,family="poisson")
summary(model2)

keeps2 <- c('iyear', 'extended', 'suicide', 'ishostkid', 'weaptype1','attacktype1', 'targtype1')
df_2<- df_terror[keeps2]
df_2$y <- df_terror$nkill
df_2[is.na(df_2)] <- 0
model3 <- glm(data = pca_df, y ~ .,method = "poisson")
str(df_2)

summary(model3)


