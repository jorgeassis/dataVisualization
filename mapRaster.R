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

cExtent <- extent(c(-180,180,30,90))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster map

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/World LGM/LandMass_GLobal_LGM_Polygon_3.shp"

iceMapMin <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ice/Ice_PresentMin.shp"
iceMapMax <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ice/Ice_PresentMax.shp"

rasterMap <- "/Volumes/Jellyfish/Dropbox/Manuscripts/_ Under Revision/Genetic and niche differentiation in the amphiboreal alga Fucus distichus/Results/BRTReclassMasked.ENSEMBLE.Present.tif"

titleMap <-  "" 
subtitleMap <-  ""
captionMap <- "Present (2000-2017)"
fillLabel <- "Probability"

elementsToHide <- c("legend","graticulate") # legend graticulate
addLabelsPlot()

## -----------------

if(class(worldMap) == "character") { worldMap <- shapefile(worldMap)}
worldMap <- crop(worldMap,cExtent)
# worldMap <- aggregate(worldMap,dissolve=T)

rasterMap <- crop( raster(rasterMap), cExtent )

iceMapMin <- crop(gBuffer(shapefile(iceMapMin), byid=TRUE, width=0),cExtent)
iceMapMax <- crop(gBuffer(shapefile(iceMapMax), byid=TRUE, width=0),cExtent)

## -----------------

rasterMap[rasterMap <= 0.2] <- NA
rasterMap <- rasterMap / max(getValues(rasterMap),na.rm=T)
rasterMap <- aggregate(rasterMap,3)

rasterMap[!is.na(rasterMap)] <- 1

## -----------------

panelColor <- "#ffffff"
oceanColor <- "#ffffff"

plot1 <-  ggplot() +
  
          #geom_polygon(data = iceMapMax, aes(x = long, y = lat, group = group), fill="#c4d4e9", colour = NA) +
          #geom_path(data = iceMapMax, aes(x = long, y = lat, group = group), color = "#c4d4e9", size = 0.1) +
          
          geom_polygon(data = iceMapMin, aes( x = long, y = lat, group = group), fill="#9dbeeb", colour = NA) +
          geom_path(data = iceMapMin, aes(x = long, y = lat, group = group), color = "#9dbeeb", size = 0.1) +
          
          geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#ded9cd", colour = NA) +
          geom_path(data = worldMap, aes(x = long, y = lat, group = group), color = "#6d6d6d", size = 0.1) +
          
          geom_tile(data = gplot_data(rasterMap), aes(x = x, y = y, fill = value)) +
  
          scale_fill_gradientn(colours=c("#a90707"), na.value='transparent',limits=c(1,1)) + # BINOMIAL
          # scale_fill_gradientn(colours=cColors, na.value='transparent',limits=c(0.2,1)) + # Continuous
          
          coord_equal() + theme_map() + labelsPlot

plot1

# Multiple Plots

grid.arrange(plot3, plot2, plot1, ncol=3) # 3: 14/8
grid.arrange(plot1, plot2, plot3, ncol=3) # 3: 14/8

grid.arrange(plot1, plot2, ncol=2) # 3: 10/8
grid.arrange(plot1, plot2, nrow=2) # 3: 10/14

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code