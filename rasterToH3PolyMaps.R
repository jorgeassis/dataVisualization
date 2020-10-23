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
source("https://raw.githubusercontent.com/jorgeassis/rGIS/master/Dependencies/mainFunctions.R")
library(ggnewscale)
library(h3r)
library(h3)
library(sf)

theme_map <- 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    #axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA), 
    panel.background = element_rect(fill = "#ffffff", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  )

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

worldMap <- ne_countries(scale = 10, returnclass = "sf")
worldMap <- st_crop(worldMap,extent(c(-180,180,-20,90)))

occurrenceRecords <- read.csv("Data/Occurrences/Solea solea/Solea_solea_un Processed.csv",sep=";")

rasters <- list.files("Results/Solea solea",full.names = TRUE,pattern="tif")
rasters
file <- 3
rasterMap <- raster(rasters[file])
rasterMap[rasterMap == 0.2] <- NA

## --------------

rasterMapDF <- data.frame(xyFromCell(rasterMap, Which( !is.na(rasterMap) , cells=T)),val=rasterMap[Which( !is.na(rasterMap) , cells=T)])
rasterMapDF <- data.frame(rasterMapDF,hex=apply(rasterMapDF[,1:2],1,function(x) { getIndexFromCoords(x[[2]], x[[1]], resolution = 4) } ))
rasterMapDF <- data.frame(hex=unique(rasterMapDF$hex),val=sapply(unique(rasterMapDF$hex),function(x) { mean(rasterMapDF[rasterMapDF$hex == x , "val"]) } ))
rasterMapDF.polygons <- h3_to_geo_boundary_sf(rasterMapDF$hex)
rasterMapDF.polygons$Value <- rasterMapDF$val

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------

captionMap <- "a) Occurrence records" 
captionMap <- "b) Present-day probability of occurrence" 
captionMap <- "c) Future probability of occurrence [RCP26]" 
captionMap <- "d) Future probability of occurrence [RCP85]" 

## -----------------

disp_win_wgs84 <- st_sfc(st_point(c(-100, 20)), st_point(c(100, 80)), crs =4326)
disp_win_trans <- st_transform(disp_win_wgs84, crs = "+proj=aea +lat_1=20 +lat_2=90 +lon_0=-10")
disp_win_coord <- st_coordinates(disp_win_trans)

occurrenceRecords <- st_as_sf(x = occurrenceRecords,coords = c("Lon", "Lat"), crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
occurrenceRecords <- st_transform(occurrenceRecords, crs = "+proj=aea +lat_1=20 +lat_2=90 +lon_0=-10")

plot1 <- ggplot() +
  geom_sf(data = worldMap,fill="#DEDEDE", colour = "#B6B6B6" , size=0.1 ) +
  geom_sf(data = occurrenceRecords, aes( colour = "#933648") ) +
  coord_sf(crs = "+proj=aea +lat_1=20 +lat_2=90 +lon_0=-25",xlim = c(-5805682,5555682), ylim = c( 1001150 ,10855985 )) + theme_map +
  annotate(geom="text", x=-5905682, y=911150, label=captionMap,size=5,family="Helvetica", color = "#000000",hjust = 0)
plot1

plot4 <- ggplot() +
         geom_sf(data = worldMap,fill="#DEDEDE", colour = "#B6B6B6" , size=0.1 ) +
         geom_sf(data = rasterMapDF.polygons, aes(fill = Value) , size=0.1) +
         scale_fill_viridis_c(option = 'magma', direction = -1, begin = 0, end = 1,) +
         coord_sf(crs = "+proj=aea +lat_1=20 +lat_2=90 +lon_0=-25",xlim = c(-5805682,5555682), ylim = c( 1001150 ,10855985 )) + theme_map +
         annotate(geom="text", x=-5905682, y=911150, label=captionMap,size=5,family="Helvetica", color = "#000000",hjust = 0)
plot2

# ----------------------

plot1.i <- plot1 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot2.i <- plot2 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot3.i <- plot3 + theme(plot.margin = unit(c(0,0,0,0), "cm"))
plot4.i <- plot4 + theme(plot.margin = unit(c(0,0,0,0), "cm"))

# ----------------------

grid.arrange(plot1.i,plot2.i,plot3.i,plot4.i, ncol = 2, nrow = 2)

pdf(file="Sup.Solea solea.pdf",width=12,height=12,useDingbats=FALSE)
grid.arrange(plot1.i,plot2.i,plot3.i,plot4.i, ncol = 2, nrow = 2)
dev.off()

