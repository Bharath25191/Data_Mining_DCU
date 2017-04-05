library(dplyr)
library(ggplot2)
library(maps)
# Describe the Dataset
df_terror <- read.csv("C:\\Users\\Lenovo\\Desktop\\sem2\\ca683_data_mining\\projects\\gtd\\globalterrorism.csv",stringsAsFactors = FALSE)
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

#5 The locations of terrorist incidents around the world
world <- map_data("world")
ggplot()+
  geom_polygon(data = world, aes(x =  long, y = lat, group = group))+  
  geom_point(data = df_terror, aes(x = longitude, y = latitude, colour = country_txt ),size=0.03,show.legend = FALSE)

#Function to multiplot the attacks on different regions
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
  
  
  set.seed(1492) 
  
  l <- layout.fruchterman.reingold(net, niter=5000, area=vcount(net)^10*10)
  
  plot(net,  layout=l,
       edge.arrow.size=.5, 
       vertex.label.cex=0.75, 
       vertex.label.family="Helvetica",
       vertex.label.font=2,
       vertex.shape="circle", 
       vertex.size=30, 
       vertex.label.color="black", 
       edge.curved=.1)
  
  ggplot()
