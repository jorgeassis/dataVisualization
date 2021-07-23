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
source("../GitGIS/Dependencies/mainFunctions.R")
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
    # legend.position = "none",
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F3F3F3", color = NA), 
    panel.background = element_rect(fill = "#F3F3F3", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank()
  )

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

mainDirectory <- "../Results/"

## ------------------------
# Global projection

mapExtent = c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)

worldMap <- ne_countries(scale = 10, returnclass = "sf")
worldMapCoordRef <- crs(worldMap)

mainGlobalMap <- ggplot() + 
  geom_sf(data = worldMap,fill="#C9C9C9", colour = "#A6A6A6" , size=0.1 ) +
  theme(axis.ticks=element_blank()) + ylab("") + xlab("") +
  theme_map + coord_sf(crs = worldMapCoordRef)
mainGlobalMap

## ------------------------
# Europe projection

worldMap <- ne_countries(scale = 10, returnclass = "sf")

mapExtent = c(xmin = -35, ymin = 20, xmax = 50.05, ymax = 85)
worldMap <- worldMap[worldMap$continent != "North America",]
worldMapCoordRef <- paste0("+proj=laea +lat_0=",mapExtent[4]-mapExtent[3]," +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

worldMap <- st_crop(worldMap,mapExtent)

mainGlobalMap <- ggplot() + 
  geom_sf(data = worldMap,fill="#ABABAB", colour = "#F3F3F3" , size=0.1 ) +
  theme(axis.ticks=element_blank()) + ylab("") + xlab("") +
  theme_map + coord_sf(crs = worldMapCoordRef)
mainGlobalMap

## ------------------------
# N Pole projection

worldMap <- ne_countries(scale = 10, returnclass = "sp")
mapExtent = c(xmin = -180, ymin = 45, xmax = 180, ymax = 90)
worldMap <- crop(worldMap,extent(mapExtent["xmin"],mapExtent["xmax"],mapExtent["ymin"],mapExtent["ymax"]))
worldMapCoordRef <- crs(worldMap)
  
x_lines <- seq(-120,180, by = 60)

mainGlobalMap <- ggplot() + 
  geom_polygon(data = worldMap,aes(x = long, y = lat, group = group),fill="#ABABAB", colour = "#F3F3F3" , size=0.1 ) +
  theme(axis.ticks=element_blank()) + ylab("") + xlab("") +
  theme_map + coord_map("ortho", orientation = c(90, 0, 0)) +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Helvetica", color = "#22211d")) +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = NA),panel.background = element_rect(fill = "#FFFFFF", color = NA), axis.ticks=element_blank()) +
  scale_y_continuous(breaks = seq(45, 90, by = 10), labels = NULL) +
  scale_x_continuous(breaks = NULL) + xlab("") +  ylab("") +
  geom_hline(aes(yintercept = 45), size = 0.5, colour = "gray")  +
  geom_segment(size = 0.1,aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed")
  
mainGlobalMap

## ------------

rasters <- list.files(mainDirectory,full.names = TRUE,pattern="tif")
rasters

resultsDirectory <- "../Results/"
mapName <- "Present"
file <- 8
rasterMap <- raster(rasters[file])
names(rasterMap)
rasterMap[rasterMap==0] <- NA

## --------------

rasters <- list.files(mainDirectory,full.names = TRUE,pattern="tif")
rasters

resultsDirectory <- "../Results/"
mapName <- "DiffRCP85"

file <- 1 
rasterMapP <- raster(rasters[file]); names(rasterMapP)

file <- 5 # 3 5
rasterMapF <- raster(rasters[file]); names(rasterMapF)

rasterMap <- rasterMapF - rasterMapP
rasterMap

## --------------

rasters <- list.files(mainDirectory,full.names = TRUE,pattern="tif")
rasters

resultsDirectory <- "../Results/"
mapName <- "LG.RCP26"

file <- 17
rasterMapP <- raster(rasters[file]); names(rasterMapP)

file <- 23 # 20 23
rasterMapF <- raster(rasters[file]); names(rasterMapF)

f1 <- function(x) { ifelse( x[[1]] == 0 & x[[2]] == 1 , 1 , ifelse(  x[[1]] == 1 & x[[2]] == 1 , 0 , ifelse(  x[[1]] == 1 & x[[2]] == 0 , -1 , NA ) ))  }
rasterMap <- calc(stack(rasterMapP,rasterMapF), fun=f1)

## --------------

# https://github.com/uber/h3/blob/master/docs/core-library/restable.md
resolutionH3 <- 3
rasterMap <- crop(rasterMap,extent(c(-175,175,-90,90)))

rasterMapDF <- data.frame(xyFromCell(rasterMap, Which( !is.na(rasterMap) , cells=T)),val=rasterMap[Which( !is.na(rasterMap) , cells=T)])
rasterMapDF <- data.frame(rasterMapDF,hex=apply(rasterMapDF[,1:2],1,function(x) { h3js::h3_geo_to_h3(x[[2]], x[[1]], res = resolutionH3) } ))
rasterMapDF <- data.frame(hex=unique(rasterMapDF$hex),val=sapply(unique(rasterMapDF$hex),function(x) { max(rasterMapDF[rasterMapDF$hex == x , "val"],na.rm=T) } ))
rasterMapDF.polygons <- h3jsr::h3_to_polygon(rasterMapDF$hex)
rasterMapDF.polygons <- st_sf(rasterMapDF.polygons)

rasterMapDF$val[rasterMapDF$val > max(modelDataset$resp)] <- max(modelDataset$resp)

rasterMapDF.polygons$hex <- as.character(rasterMapDF$hex)
rasterMapDF.polygons$value <- as.numeric(as.character(rasterMapDF$val))

## ----------------------------------------------------------------------------------------------------

rasterMapDF.polygons <- sf:::as_Spatial(rasterMapDF.polygons)
rasterMapDF.polygons$value <- as.numeric(as.character(rasterMapDF.polygons$value))
rasterMapDF.polygons$hex <- as.character(rasterMapDF.polygons$hex)

rasterMapDF.polygons <- fortify(rasterMapDF.polygons)
rasterMapDF.polygons <- rasterMapDF.polygons[rasterMapDF.polygons$lat >= extent(worldMap)[3],]
rasterMapDF.polygons$value <- sapply(as.numeric(rasterMapDF.polygons$id), function(x) { rasterMapDF[x,"val"] })


rasterMapDF.polygons <- st_wrap_dateline(rasterMapDF.polygons, options = "WRAPDATELINE=YES", quiet = TRUE)


rasterMapDF.polygons$value[rasterMapDF.polygons$value < 0] <- 0


minLegend <- min(rasterMapDF.polygons$value); minLegend
maxLegend <- max(rasterMapDF.polygons$value); maxLegend

#----------------------
# Continuous Map 
# Light green 82DC9D; Light orange F58B12; Light Blue 11BEE9; Light purple D444C1; Dark purple 8D007A

# "#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414" // c(0,0.2,0.4,0.6,0.8,1)

myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414")

#----------------------

plot1 <- mainGlobalMap +
  scale_colour_gradientn(colours = myColors, values = seq(0,1,length.out=6), n.breaks = 6,labels=round(seq((minLegend),(maxLegend), length.out=6), digits=2), guide = "colourbar", aesthetics = "fill", limits=c(minLegend,maxLegend) ) +
  geom_polygon(data = rasterMapDF.polygons, aes(x = long, y = lat,group=id, fill=value), colour = NA, size = 0) +
  theme(legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.75, 'cm'),
        legend.background = element_rect(fill = "#F3F3F3", color = NA)) + theme(legend.title=element_blank())

plot1

#----------------------
# Continuous Map 
# Light green 82DC9D; Light orange F58B12; Light Blue 11BEE9; Light purple D444C1; Dark purple 8D007A

myColors <- c("#8D007A","#FFFFFF","#F58B12") 
myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414") # FCBA49

#----------------------

plot1 <- mainGlobalMap +
  
  # Classes
  geom_polygon(data = rasterMapDF.polygons, aes(x = long, y = lat,group=id, fill=value), colour ="black", fill= myColors[rasterMapDF.polygons$value], size = 0.1) +
  
  # Continuous
  #geom_polygon(data = rasterMapDF.polygons, aes(x = long, y = lat,group=id, fill=value), colour ="black", size = 0.1) +
  #scale_colour_gradient2(low = myColors[1], mid = myColors[2], high = myColors[3], guide = "colourbar", aesthetics = "fill", limits=c(minLegend,maxLegend)) +

  # Continuous
  #geom_polygon(data = rasterMapDF.polygons, aes(x = long, y = lat,group=id, fill=value), colour ="black", size = 0.1) +
  #scale_colour_gradient2(low = myColors[1], mid = myColors[2], high = myColors[3], guide = "colourbar", aesthetics = "fill", limits=c(minLegend,maxLegend)) +
  #scale_colour_gradientn(colours = myColors, values = c(0,0.2,0.4,0.6,0.8,1), guide = "colourbar", aesthetics = "fill", limits=c(minLegend,maxLegend)) +

  
  # Continuous (Negative / Positive)
  # geom_polygon(data = rasterMapDF.polygons[rasterMapDF.polygons$value >= 0,], aes(x = long, y = lat,group=id, fill=value), colour ="black", size = 0.1) +
  # scale_colour_gradient2(low = myColors[2], high = myColors[3], guide = "colourbar", aesthetics = "fill", limits=c(0,maxLegend)) +
  # new_scale("fill") +
  # geom_polygon(data = rasterMapDF.polygons[rasterMapDF.polygons$value < 0,], aes(x = long, y = lat,group=id, fill=value), colour ="black", size = 0.1) +
  # scale_colour_gradient2(low = myColors[1], high = myColors[2], guide = "colourbar", aesthetics = "fill", limits=c(minLegend,0)) +
    
  theme(legend.position=c(.99, .99), # legend.position=c(.99, .99)
        legend.justification=c("right", "top"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-80),legend.title=element_blank(),legend.background = element_rect(fill = "#ffffff", color = NA))

plot1

pdf(file=paste0(resultsDirectory,mapName,".pdf"),width=12,useDingbats=FALSE)
plot1
dev.off()

## --------------------------------------------
## --------------------------------------------

plot1.i <- plot1 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot2.i <- plot2 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot3.i <- plot3 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))

# ----------------------

plotCombined <- grid.arrange(plot1.i, plot2.i, plot3.i, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined) + theme(plot.background = element_rect(fill="#ffffff", color = NA))
plotCombined

pdf(file=paste0("../../Estimating future distributional shifts of Arctic marine macroalgae/Figures/KelpsFig1.pdf"),width=12,useDingbats=FALSE)
plotCombined
dev.off()

pdf(file=paste0("../../Estimating future distributional shifts of Arctic marine macroalgae/Figures/KelpsLegend.pdf"),width=12,useDingbats=FALSE)
plotLegend
dev.off()
