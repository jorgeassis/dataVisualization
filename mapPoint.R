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

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

cExtent <- extent(c(100,165,0,62.5))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Point data Map

pointData <- "~/Dropbox/Manuscripts/The Modelling Factory/Kelp/2. Laminaria abyssalis/Data/Occurrences/lp.csv"
pointData <- read.table(pointData,sep=";",header=T)[,c("Lon","Lat")]

## -----------------

worldMap <- crop(worldMap,cExtent)

buffer <- c(1,1,1,1)

ggplot() + geom_point(data = pointData, aes(x = Lon, y = Lat)) + 
  coord_fixed(xlim = c(min(pointData$Lon) - buffer[1], max(pointData$Lon) + buffer[2]), ylim = c(min(pointData$Lat) - buffer[3], max(pointData$Lat) + buffer[4])) +
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="lightgray", colour = NA) +
  geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "lightgray", size = 0.1) +
  coord_equal() +
  theme_map() +
  labs(
    #x = "Longitude", 
    #y = "Latitude", 
    title = "Distribution records of Laminaria abyssalis", 
    subtitle = "Point data (occurrences)") + theme(legend.position = "none")                                                                                                                                    

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code