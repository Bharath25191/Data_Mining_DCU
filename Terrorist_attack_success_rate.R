library(dplyr)
library(ggplot2)

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
df_failure$success <- NULL

ys= df_success %>% select(success)
df_success$success <- NULL
