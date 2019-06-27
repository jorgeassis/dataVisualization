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

cExtent <- extent(c(100,165,0,62.5))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")
panelColor <- "#ffffff"
oceanColor <- "#ffffff"

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Point data Map

pointData <- "/Volumes/Jellyfish/Dropbox/Manuscripts/_ Under Revision/The centre-periphery hypothesis in the Northwest Pacific/Data/Occurrence/Sargassum.final.records.txt"
pointData <- read.table(pointData,sep=",",header=T)[,c("Lon","Lat")]

north.group <- pointData[pointData$Lat > 30,]
south.group <- pointData[pointData$Lat < 30,]

titleMap <-  "" 
subtitleMap <-  ""
captionMap <- "Distribution records used for modelling"
fillLabel <- "Probability"

elementsToHide <- c("legend","graticulate") # legend graticulate
addLabelsPlot()

## -----------------

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- crop(worldMap,cExtent)

buffer <- c(1,1,1,1)

Map1 <- ggplot() +
        geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#ded9cd", colour = NA) +
        geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "#767676", size = 0.1) +
        geom_point(data = north.group, aes(x = Lon, y = Lat), color = "#a90707") + 
        geom_point(data = south.group, aes(x = Lon, y = Lat), color = "#a90707") + 
        coord_equal() +
        theme_map() + labelsPlot

Map1 # 8/8

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code