#Exploratory Analysis for Terrorist Groups:
#Date: 02:04:2017

library(dplyr)
library(ggplot2)
library(maps)
library(doBy)
library(plotly)
library(plyr)
library(dplyr)

# Load the Dataset
df_terror <- read.csv("C:\\Users\\Lenovo\\Desktop\\sem2\\ca683_data_mining\\projects\\gtd\\globalterrorism.csv",stringsAsFactors = FALSE)
# Dimensions
dim(df_terror)
# The top 6 records of the table
head(df_terror, 2)
# A concise summary of the table using the dplyr package function 
glimpse(df_terror)
# Summary Statistics of the Terror dataset
summary(df_terror)

# Names of the fields
names(df_terror)


#Better understand the terrorist perpetrators

#Check the factors
alt <- levels(factor(df_terror$gname))
alt

#1. Who are they?
#install.packages("plotly")


#Sort the Terrorist Groups in Descending order and get the top 16 groups according to the 
#Incidents 
groups = as.data.frame(head(sort(table(df_terror$gname),decreasing=TRUE),16))
#Remove the Unknow Group
groups<- groups[-1,]
#Check if the unknown group has been deleted and the remaining top 15 groups are present 
groups
groups$Var1 = with(groups, factor(Var1, levels = rev(levels(Var1))))
#Plot the Top 15 terrorist groups with More incidents using GGPLOT
ggplot(groups, aes(x=Var1, y=Freq))+
  geom_bar(stat='identity',fill='blue')+
  coord_flip()+
  ggtitle("Top 15 groups with more incidents") +
  labs(x="Terrorist_Groups",y="Incidents")
  ggtitle("Terrorist Attacks by Country") +
  labs(x="Country",y="Number of Attacks") 
  
  
  
#2.Where are they from?
country = as.data.frame(head(sort(table(df_terror$country_txt),decreasing=TRUE),15))
country
country$Var1 = with(country, factor(Var1, levels = rev(levels(Var1))))
#Plot Top countries with More incidents using GGPLOT
ggplot(country, aes(x=Var1, y=Freq))+
  geom_bar(stat='identity',fill='blue')+
  coord_flip()+
  ggtitle("Top countries with more incidents") +
  labs(x="Countries",y="Incidents")



#3.Do they act in groups? Are they working alone?
Act1 <- data.frame( df_terror$region_txt,
                   df_terror$nperps)
Act<-Act1[Act1$df_terror.nperps >=0,]
Clean_Act<- na.omit(Act)

#Median Perpetrators
Median_perp <- aggregate( df_terror.nperps ~ df_terror.region_txt, Act, median )
typeof(Median_perp)
Median_prep<-as.data.frame(Median_perp)
Median_perp

#Plot the data using GGPLOT
ggplot(Median_prep, aes(x=df_terror.region_txt, y=df_terror.nperps))+
  geom_bar(stat='identity', fill='blue')+
  coord_flip()+
  ggtitle("Top Regions with median highest number of perpetrators in one attack ") +
  labs(x="Region",y="No. of Perpetrators")



#4.Are they captured? Does that depend on region?
capture <- c('nperps','nperpcap','region_txt')
df_capture <- df_terror[capture]
df_capture <- df_capture[complete.cases(df_capture),]
df_capture<- df_capture[(df_capture$nperpcap > 0),]
df_capture<- df_capture[(df_capture$nperps > 0),]


percent <- (df_capture$nperpcap/(df_capture$nperps))
df_capture<- cbind(df_capture, percent)
head(df_capture)

Captured<- df_capture %>% 
  arrange(region_txt)%>%
  group_by(region_txt)

df_captured<- aggregate(Captured$percent, by=list(Region=Captured$region_txt), FUN=sum)

#Plot the data using GGPLOT
ggplot(df_captured, aes(x=Region, y=x/1000))+
  geom_bar(stat = "identity",fill='blue')+
  coord_flip()+
  ggtitle("Percentage of terrorist captures in each region") +
  labs(x="Region",y="Percentage of Terrorists Captured")

#5.Map of the activities
map <- map_data("world")
ggplot()+
  geom_polygon(data = world, aes(x =  long, y = lat, group = group))+ 
  geom_point(data = df_terror, aes(x = longitude, y = latitude, colour = country_txt ),size=0.03,show.legend = FALSE)

#6. Function to multiplot the attacks on different regions
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

d1 <- df_terror[df_terror$country_txt == "United States", ] 
d2 <- df_terror[df_terror$country_txt == "India", ] 
d3 <- df_terror[df_terror$country_txt == "Iraq", ] 
d4 <- df_terror[df_terror$country_txt == "Egypt", ] 


p2 <- ggplot(d1, aes(x=iyear)) + 
  #geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  geom_density(fill = "#0080ff", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  labs(x="Year", y="Density", title="Attacks on United States")

p3 <- ggplot(d2, aes(x=iyear)) + 
  #geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  geom_density(fill = "#8000ff", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  labs(x="Year", y="Density", title="Attacks on India")

p4 <- ggplot(d3, aes(x=iyear)) + 
  #geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  geom_density(fill = "#ff0080", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  labs(x="Year", y="Density", title="Attacks on Iraq")

p5 <- ggplot(d4, aes(x=iyear)) + 
  #geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  geom_density(fill = "#ffbf00", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  labs(x="Year", y="Density", title="Attacks on Egypt")

multiplot(p2, p3, p4, p5, cols=2)







  
  
  
  
