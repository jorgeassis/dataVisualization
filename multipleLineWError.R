## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

library(ggplot2)
dataset <- read.csv("Data/Dataset1.csv")

dataset <- data.frame(year=rep(timeSteps,3),
                      Scenario=c(rep("SSP119",length(valueSSP119)),rep("SSP460",length(valueSSP460)),rep("SSP585",length(valueSSP585))),
                      Values=c(valueSSP119,valueSSP460,valueSSP585),
                      ValuesSD=c(valueSSP119SD,valueSSP460SD,valueSSP585SD))

mainTheme <- theme(panel.grid.major = element_blank() ,
                   text = element_text(size=12) ,
                   axis.title.y = element_text(margin = margin(t = 0, r = 16, b = 0, l = 0)) ,
                   axis.title.x = element_text(margin = margin(t = 16, r = 0, b = 0, l = 0)) ,
                   legend.title = element_blank() ,
                   legend.margin=margin(c(0.3,1,0.3,1), unit='lines') ,
                   legend.background = element_rect(fill="white", size=0.2, linetype="solid",  colour ="#979797"))

mainLab <- "Temperatura superficial do oceano (ºC)"

plot1 <- ggplot(data=dataset , aes(x=year, y=Values)) + 
  
  scale_x_continuous("Time (year)", labels = as.character(seq(min(dataset$year),max(dataset$year),by=10)), breaks = seq(min(dataset$year),max(dataset$year),by=10)) +
  
  geom_ribbon(aes(ymin = Values - ValuesSD / 2, ymax = Values + ValuesSD / 2,fill=Scenario), linetype = 1, size = 0, colour = "#B8D6DC", alpha=1, show.legend = F) +
  geom_line(aes(color = Scenario, linetype=Scenario)) +
  
  scale_color_manual(values = c("#C9C9C9","#979797","#A1A1A1","#3C3C3C")) +
  scale_fill_manual(values = c("#DCF2F6","#DCF2F6","#DCF2F6","#DCF2F6")) +
  
  scale_linetype_manual(values = c('solid','solid','dashed', 'solid')) +
  
  theme_minimal(base_size = 14) + mainTheme + 
  ylab(mainLab) +
  theme(legend.position = c(0.15, 0.2)) # c(0.15, 0.8) c(0.15, 0.2) 
plot1
