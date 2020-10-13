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
source("Dependencies/mainFunctions.R")
library(ggnewscale)

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

cExtent <- extent(c(10,30,-38,-12))
cExtent <- getMapExtent()

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")
cColorsGray <- c("#868686", "#000000")
panelColor <- "#ffffff"
oceanColor <- "#ffffff"

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster map

worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- "../../../Data/Shapefiles/World LGM/LandMass_GLobal_LGM_Polygon_3.shp"

oceanMap <- "../../../Data/Rasters/Ocean Raster/GRAY_HR_SR_OB.tif"

rasterMap <- "../Results/SDM/EnsembleReclass.Present.tif"
rasterMap <- "../Results/SDM/EnsembleReclass.MH.Masked.tif"
rasterMap <- "../Results/SDM/EnsembleReclass.LGM.Masked.tif"

iceMapMin <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ice/Ice_PresentMin.shp"
iceMapMax <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/Global Ice/Ice_PresentMax.shp"

titleMap <-  "" 
subtitleMap <-  ""
captionMap <- "Mid-Holocene (~6kybp)" # Last Interglacial (~120kybp) Present (2000-2017)
fillLabel <- "Potential distribution"

elementsToHide <- c("legend","graticulate") # legend graticulate
addLabelsPlot()

## -----------------

if(class(worldMap) == "character") { worldMap <- shapefile(worldMap)}
worldMap <- crop(worldMap,cExtent)
worldMap <- aggregate(worldMap,dissolve=T)

rasterMap <- crop( raster(rasterMap), cExtent )
oceanMap <- crop( raster(oceanMap), cExtent )

#iceMapMin <- crop(gBuffer(shapefile(iceMapMin), byid=TRUE, width=0),cExtent)
#iceMapMax <- crop(gBuffer(shapefile(iceMapMax), byid=TRUE, width=0),cExtent)

## -----------------

rasterMap[rasterMap <= 0.2] <- NA
rasterMap <- rasterMap / max(getValues(rasterMap),na.rm=T)
rasterMap[!is.na(rasterMap)] <- 1

# rasterMap <- aggregate(rasterMap,3)

## -----------------

theme_map <- 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_blank(), #element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA), 
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  )


plot2 <- ggplot() +
  
  geom_tile(data = gplot_data(oceanMap), aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours=cColorsGray, limits=c(69,150)) + # Continuous
  
  new_scale("fill") +
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#D2D2D2", colour = "#D2D2D2" , size=0.25 ) +
  
  geom_tile(data = gplot_data(rasterMap), aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours=c("#F30000"), na.value='transparent',limits=c(1,1)) + # BINOMIAL
  
  geom_segment(colour = "Black" , size = 0.15,aes(y = cExtent[3]-1, yend = cExtent[4]+1, x = seq(cExtent[1],cExtent[2], by = 5)[2:4], xend = seq(cExtent[1],cExtent[2], by = 5)[2:4]), linetype = "dashed") +
  geom_segment(colour = "Black" , size = 0.15,aes(y = seq(cExtent[3],cExtent[4], by = 5)[2:5], yend =seq(cExtent[3],cExtent[4], by = 5)[2:5], x = cExtent[1]-1, xend = cExtent[2]+1), linetype = "dashed") +
  
  coord_equal() + # coord_map('lambert', lat0=extent(worldMap)[4], lat1=extent(worldMap)[3]) + 
  theme_map + theme(legend.position="none") +
  annotate(geom="text", x=cExtent[2]-cExtent[1], y=cExtent[3]-2, label=captionMap,size=6,family="Helvetica", color = "#7F7F7F",hjust = 0.5)

plot2

# Multiple Plot

pdf(file="Plot.pdf",width=15,useDingbats=FALSE)
grid.arrange(plot1,plot2,plot3, ncol = 3)
dev.off()

grid.arrange(plotLIG, plotLGM, plotMH,plotP, ncol=4) # 3: 18/8
grid.arrange(plot3, plot2, plot1, ncol=3) # 3: 14/8
grid.arrange(plot3, plot3, ncol=2) # 2: 10/8

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code