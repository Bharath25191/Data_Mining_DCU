library(dplyr)
library(ggplot2)
library(maps)
# Describe the Dataset
df_terror <- read.csv("C:\\DCU\\Data Mining\\Mini Projects\\globalterrorismdb.csv",stringsAsFactors = FALSE)
# Dimensions
dim(df_terror)
# The top 6 records of the table
head(df_terror)
# A concise summary of the table using the dplyr package function 
glimpse(df_terror)
# Summary Statistics of the Terror dataset
summary(df_terror)

# Names of the fields
names(df_terror)

# Data Visualizations

#1 Frequency of the attacks over the years
ggplot(data= df_terror,aes(x=iyear))+
  geom_histogram(colour = "darkgreen",aes(fill=..count..))+
  scale_fill_gradient("Count", low = "yellow", high = "red")+
  ggtitle("Frequency of the attacks over the years") +
  labs(x="Year",y="Number of Attacks") 

#2 Country of the attack
library(plyr)
country_count <- count(df_terror, "country_txt")
country_count <- filter(country_count,freq > 4000)
country_count$country_txt<-reorder(country_count$country_txt,country_count$freq)
ggplot(data= country_count,aes(x=country_txt,y=freq,fill=country_txt))+
  geom_bar(stat="identity",width = 0.8)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  ggtitle("Terrorist Attacks by Country") +
  labs(x="Country",y="Number of Attacks") 

#3 Weapon Preference
weapon_count <- count(df_terror, "weaptype1_txt")
#country_count = filter(country_count,freq > 4000)
#weapon_count$weaptype1_txt<-reorder(weapon_count$weaptype1_txt,weapon_count$freq)
print(weapon_count)
weapon_count <- filter(weapon_count, !grepl('Vehicle', weaptype1_txt))
ggplot(data= weapon_count,aes(x=weaptype1_txt,y=freq,fill=weaptype1_txt))+
  geom_bar(stat="identity",width = 0.8)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  ggtitle("Weapon Type Frequency") +
  labs(x="Weapon",y="Count") 

#4 Weapon Preference
target_count <- count(df_terror,"targtype1_txt")
#weapon_count$weaptype1_txt<-reorder(weapon_count$weaptype1_txt,weapon_count$freq)
#weapon_count <- filter(weapon_count, !grepl('Vehicle', weaptype1_txt))
ggplot(data= target_count,aes(x=targtype1_txt,y=freq,fill=targtype1_txt))+
  geom_bar(stat="identity",width = 0.8)+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  ggtitle("Weapon Type Frequency") +
  labs(x="Weapon",y="Count") 

# 4
world <- map_data("world")
ggplot()+
  geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = df_terror, aes(x = longitude, y = latitude, colour = country_txt ),size=0.03,show.legend = FALSE)

