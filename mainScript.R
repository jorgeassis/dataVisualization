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

cExtent <- getMapExtent()
cExtent <- extent(c(100,165,10,65))

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

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster + Polygon map

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- "~/Dropbox/Data/Shapefiles/World LGM/LandMass GLobal LGM Polygon 2.shp"

iceMap <- "~/Dropbox/Data/Shapefiles/Global Ice/Ice_LGMMin"

rasterMap <- "~/Dropbox/Manuscripts/_ Under Revision/The centre-periphery hypothesis in the Northwest Pacific/Results/SDM/BRTReclassMasked.ENSEMBLE.LGM.tif"

titleMap <-  "" # Potential distribution of Laminaria abyssalis" # "Niche suitability (probability of occurrence)"
subtitleMap <-  "" # "Binomial reclassification (occurrence)"
captionMap <- "Last Glacial Maximum (20kybp)" # Last interglacial distribution (120kybp)
fillLabel <- "Probability"

elementsToHide <- c("legend") # legend graticulate
addLabelsPlot()

## -----------------

if(class(worldMap) == "character") { worldMap <- shapefile(worldMap)}

if(iceMap != "") { iceMap <- crop(shapefile(iceMap),cExtent) }

worldMap <- crop(worldMap,cExtent)
rasterMap <- crop( raster(rasterMap), cExtent )

## -----------------

rasterMap[rasterMap <= 0.2] <- NA
rasterMap <- rasterMap / max(getValues(rasterMap),na.rm=T)
rasterMap <- aggregate(rasterMap,2)

## -----------------

plot3 <-  ggplot() +
  
          geom_polygon(data = iceMapMax, aes(alpha = 0, x = long, y = lat, group = group), fill="white", colour = NA) +
          geom_path(data = iceMapMax, aes(alpha = 0, x = long, y = lat, group = group), color = "white", size = 0.1) +

          geom_polygon(data = iceMapMin, aes( x = long, y = lat, group = group), fill="white", colour = NA) +
          geom_path(data = iceMapMin, aes(x = long, y = lat, group = group), color = "white", size = 0.1) +
          
          geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#ded9cd", colour = NA) +
          geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "#ded9cd", size = 0.1) +
          
          geom_tile(data = gplot_data(rasterMap), aes(x = x, y = y, fill = value)) +
          scale_fill_gradientn(colours=c("#A90909"), na.value='transparent',limits=c(1,1)) + # BINOMIAL
          
          coord_equal() + theme_map() + labelsPlot

plot3

# Multiple Plots

grid.arrange(plot3, plot2, plot1, ncol=3)

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

## --------------------------------------------------
## Point map

