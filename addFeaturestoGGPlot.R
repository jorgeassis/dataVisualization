## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

library(ggplot2)

# Add title
ggtitle("Plot of length \n by dose")

# Add subtitles

labs(title = "Potential distributional range shifts for Gadus morhua", subtitle = "Scenario RCP85 [2090-2100]")

# Add multiple

labs(x = NULL, y = NULL, 
     title = "RCP85 [2090-2100]", # Present conditions [2000-2017] Mid-Holocene [6 kybp] Last Glacial Maximum [120 kybp]
     subtitle = NULL, 
     caption = NULL)

# Add horizontal line
scale_x_continuous(breaks=c(deforestation$Year[seq(1,27,by=2)]))

# Add vertical line
geom_vline(xintercept = 178.7, linetype="dotted", color = "Black", size=0.5)

# Add text
annotate(geom="text", x=2.53, y=13.6, label="Deviance Explained: 0.54", color="Black")
