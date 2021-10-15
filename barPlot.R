## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

# Projections and mapped projections

library(ggplot2)

data <- read.csv("Data/Dataset1.csv")

mainTheme <- theme(panel.grid.major = element_blank() ,
                   text = element_text(size=12) ,
                   axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0)) ,
                   axis.title.x = element_text(margin = margin(t = 16, r = 0, b = 0, l = 0)) )


plot1 <- ggplot(data, aes(x=Time, y=Value)) + 
  scale_x_continuous("Time (year)", labels = as.character(c(seq(1990,2021,by=3),2021)), breaks = c(seq(1990,2021,by=3),2021)) +
  geom_bar(stat = "identity", fill="#61A5C7") + theme_minimal(base_size = 14) + mainTheme + 
  ylab("Maximum kelp coverage (m2)") +
  annotate("segment", x = 1991, y = -40, xend = 1991, yend = -20,color="#CE6B6B",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 1992, y = -40, xend = 1992, yend = -20,color="#CE6B6B",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 1997, y = -40, xend = 1997, yend = -20,color="#8B1010",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 1998, y = -40, xend = 1998, yend = -20,color="#8B1010",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 2015, y = -40, xend = 2015, yend = -20,color="#8B1010",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 2016, y = -40, xend = 2016, yend = -20,color="#8B1010",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  
  annotate("segment", x = 1990, y = 11100, xend = 1990, yend = 11120,color="#8B1010",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("segment", x = 1990, y = 10600, xend = 1990, yend = 10620,color="#CE6B6B",arrow = arrow(type = "closed", length = unit(0.02, "npc"))) + 
  
  geom_text(data=data.frame(x = c(1991,1991),y = c(10500,11000),label = c("Very Strong El Niño", "Strong El Niño")), aes( x=x, y=y, label=label) , color="black", size=4 , angle=0 , hjust = 0)

plot1



