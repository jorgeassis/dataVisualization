## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
library(raster)
library(ggnewscale)
library(rnaturalearth)
library(ggplot2)

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

cExtent <- extent(c(-178,-88,5,65))

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster map

rasterMap <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Past distirbutional shifts of Phyllospadix along NE Pacific coastlines/ResultsPscouleri/EnsembleReclass.LGM.Masked.tif"
worldMap <- shapefile("Data/landmassLGMPolygon2.shp")
worldMap <- crop( worldMap, cExtent )

rasterMap <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Past distirbutional shifts of Phyllospadix along NE Pacific coastlines/ResultsPscouleri/EnsembleReclass.Present.tif"
rasterMap <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Past distirbutional shifts of Phyllospadix along NE Pacific coastlines/ResultsPscouleri/EnsembleReclass.RCP85.Masked.tif"
worldMap <- ne_countries(scale = 10, returnclass = "sp")
worldMap <- crop( worldMap, cExtent )

rasterMap <- crop( raster(rasterMap), cExtent )
rasterMap <- aggregate(rasterMap,2,max)
rasterMap <- as.data.frame(rasterMap,xy=TRUE,na.rm=T)
colnames(rasterMap) <- c("Lon","Lat","Val")

## -----------------

theme_map <- 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", color = NA), # F3F3F3
    panel.background = element_rect(fill = "#FFFFFF", color = NA), # F3F3F3
    legend.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank()
  )

## -----------------

# worldMapCoordRef <- paste0("+proj=laea +lat_0=",mean(cExtent[4],cExtent[3])," +lon_0=",mean(cExtent[2],cExtent[1])," +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
# worldMapCoordRef <- crs(worldMap)
# worldMapCoordRef <- CRS("+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# projectionMap <- coord_sf(crs = worldMapCoordRef) 

# projectionMap <- coord_map("azequalarea") 
# projectionMap <- coord_map('lambert', lat0=extent(worldMap)[3], lat1=extent(worldMap)[4]) 
# projectionMap <- coord_equal() 
# projectionMap <- coord_map("bonne", lat0 = 50) 
# projectionMap <- coord_map("conic", lat0 = 15) 
# projectionMap <- coord_map("gilbert")

projectionMap <- coord_map("mollweide")

## -----------------

captionMap <- "Future (2100, RCP85)"

# Continuous
myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414")

# Binomial
myColors <- c("#A41F1F")

plot3 <- ggplot() +
  
  # geom_tile(data = oceanMap, aes(x=Lon,y=Lat,fill=Val)) +
  # scale_fill_gradientn(colours=cColorsGray, limits=c(69,150)) + # Continuous
  # new_scale("fill") +
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#D2D2D2", colour = "#D2D2D2" , size=0.25 ) +
  geom_tile(data = rasterMap, aes(x=Lon,y=Lat,fill=Val)) +
  scale_fill_gradientn(colours=myColors, na.value='transparent',limits=c(1,1)) + # ,limits=c(1,1) BINOMIAL

  projectionMap +
  
  theme_map + 
  theme(legend.position="None", # "bottom
        legend.margin=margin(0,0,0,0),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.75, 'cm'),
        legend.background = element_rect(fill = "#F3F3F3", color = NA)) + theme(legend.title=element_blank()) +
  annotate(geom="text", x=cExtent[1]+2, y=cExtent[3]+8, label=captionMap,size=4.25,family="Helvetica", color = "#444444",hjust = 0)

plot3

# --------------------
# Multiple Plot

library(gridExtra)
pdf(file="Plot.pdf",width=15,useDingbats=FALSE)
grid.arrange(plot1,plot2,plot3, ncol = 3)
dev.off()

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code