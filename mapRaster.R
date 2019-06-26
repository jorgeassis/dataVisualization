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
## Raster map

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- "~/Dropbox/Data/Shapefiles/World LGM/LandMass GLobal LGM Polygon 2.shp"

iceMapMin <- "~/Dropbox/Data/Shapefiles/Global Ice/Ice_2100RCP26Min.shp"
iceMapMax <- "~/Dropbox/Data/Shapefiles/Global Ice/Ice_2100RCP26Max.shp"

rasterMap <- "~/Dropbox/Manuscripts/_ Under Revision/The centre-periphery hypothesis in the Northwest Pacific/Results/SDM/BRTReclassMasked.ENSEMBLE.RCP26.tif"

titleMap <-  "" # Potential distribution of Laminaria abyssalis" # "Niche suitability (probability of occurrence)"
subtitleMap <-  "" # "Binomial reclassification (occurrence)"
captionMap <- "Future RCP26 (2090-2100)" # Last interglacial distribution (120kybp)
fillLabel <- "Probability"

elementsToHide <- c("legend","graticulate") # legend graticulate
addLabelsPlot()

## -----------------

if(class(worldMap) == "character") { worldMap <- shapefile(worldMap)}

iceMapMin <- crop(gBuffer(shapefile(iceMapMin), byid=TRUE, width=0),cExtent)
iceMapMax <- crop(gBuffer(shapefile(iceMapMax), byid=TRUE, width=0),cExtent)

worldMap <- crop(worldMap,cExtent)
worldMap <- aggregate(worldMap,dissolve=T)

rasterMap <- crop( raster(rasterMap), cExtent )

## -----------------

rasterMap[rasterMap <= 0.2] <- NA
rasterMap <- rasterMap / max(getValues(rasterMap),na.rm=T)
rasterMap <- aggregate(rasterMap,3)

## -----------------

panelColor <- "#ffffff"
oceanColor <- "#ffffff"

plot2 <-  ggplot() +
  
          geom_polygon(data = iceMapMax, aes(x = long, y = lat, group = group), fill="#c4d4e9", colour = NA) +
          geom_path(data = iceMapMax, aes(x = long, y = lat, group = group), color = "#c4d4e9", size = 0.1) +
  
          geom_polygon(data = iceMapMin, aes( x = long, y = lat, group = group), fill="#9dbeeb", colour = NA) +
          geom_path(data = iceMapMin, aes(x = long, y = lat, group = group), color = "#9dbeeb", size = 0.1) +
          
          geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#ded9cd", colour = NA) +
          geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "#767676", size = 0.1) +
          
          geom_tile(data = gplot_data(rasterMap), aes(x = x, y = y, fill = value)) +
          scale_fill_gradientn(colours=c("#a90707"), na.value='transparent',limits=c(1,1)) + # BINOMIAL
          
          coord_equal() + theme_map() + labelsPlot

plot2

# Multiple Plots

grid.arrange(plot3, plot2, plot1, ncol=3) # 3: 14/8
grid.arrange(plot3, plot2, plot1, ncol=3) # 3: 14/8

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code