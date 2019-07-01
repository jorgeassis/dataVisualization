## --------------------------------------------------
## --------------------------------------------------
##
## Pretty maps [R]
## Muffins 'n' Code
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

source("mainFunctions.R")

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

cExtent <- extent(c(10,30,-38,-15))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")
panelColor <- "#ffffff"
oceanColor <- "#ffffff"

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Point data Map

pointData <- "/Volumes/Jellyfish/Dropbox/Manuscripts/The Modelling Factory/Kelp/1. Laminaria pallida (Surface)/Data/Occurrences/Laminaria pallida Processed.csv"
pointData <- read.csv(pointData,sep=";",header=T)[,c("Lon","Lat")]
pointData <- pointData[pointData$Lon >= 14,]

titleMap <-  "" 
subtitleMap <-  ""
captionMap <- "Distribution records used for modelling"
fillLabel <- "Probability"

elementsToHide <- c("legend","graticulate") # legend graticulate
addLabelsPlot()

## -----------------

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- crop(worldMap,cExtent)
worldMap <- aggregate(worldMap,dissolve=T)

buffer <- c(1,1,1,1)

Map1 <- ggplot() +
        geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#ded9cd", colour = NA) +
        geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "#767676", size = 0.1) +
        geom_point(data = pointData, aes(x = Lon, y = Lat), color = "#565656") + 
        coord_equal() +
        theme_map() + labelsPlot

Map1 # 8/8

grid.arrange(Map1, Map2, ncol=2) # 2: 10/8

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code