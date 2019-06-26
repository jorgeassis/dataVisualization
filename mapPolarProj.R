## --------------------------------------------------
## --------------------------------------------------
##
## Pretty maps [R]
## Muffins 'n' Code
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

## https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

source("mainFunctions.R")

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster map with Polar projection
## ? mapproject

worldMap.c <- crop(worldMap,extent(c(-180,180,45,90)))

ggplot() + 
  geom_polygon(data = worldMap.c, aes(x = long, y = lat, group = group) , fill="lightgray", colour = "black") +
  scale_y_continuous("",breaks=NULL,labels=NULL) + 
  scale_x_continuous("",breaks=NULL,labels=NULL) +  
  coord_map("stereographic", orientation=c(90, 0, 0))

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code