library(dplyr)
library(magrittr)
library(ggplot2)
library(corrgram)
library(data.table)

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

nb.fit = glm(data = df, nkill ~ nkillus + nkillter + nwound + claimmode3 + ishostkid, family = negative.binomial(theta=1,link="identity"),start = model2$coefficients)
summary(nb.fit)

# Chi Square test to check the fit of the model
1-pchisq(328888,156771)

