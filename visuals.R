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


