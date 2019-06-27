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

cExtent <- extent(c(-50,-20,-34,0))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")
panelColor <- "#ffffff"
oceanColor <- "#ffffff"

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Point data Map

pointData <- "/Volumes/Jellyfish/Dropbox/Manuscripts/_ Under Revision/Modelling the distribution of Laminaria abyssalis/Data/lp.csv"
pointData <- read.table(pointData,sep=";",header=T)[,c("Lon","Lat")]

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
        geom_point(data = pointData, aes(x = Lon, y = Lat), color = "#565656") + 
        coord_equal() +
        theme_map() # + labelsPlot

Map1 # 8/8

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code