library(dplyr)
library(ggplot2)
globalterror  <- read.csv("C:\\DataScience_DataSets\\globalterrorismdb_0616dist.csv",stringsAsFactors = FALSE)

#attacks around the world

p <- ggplot(globalterror, aes(x=iyear)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333", binwidth = 1) + 
  geom_density(fill = "#ff4d4f", alpha = 0.5) + 
  theme(panel.background = element_rect(fill = '#ffffff')) + 
  labs(x="Year", y="Attacks", title="Attacks per Year Around the World")
p


d1 <- globalterror[globalterror$country_txt == "United States", ] 
d2 <- globalterror[globalterror$country_txt == "India", ] 
d3 <- globalterror[globalterror$country_txt == "Iraq", ] 
d4 <- globalterror[globalterror$country_txt == "Egypt", ] 


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