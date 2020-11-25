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

mainDirectory <- "../Results/_ Backup/_ Ensembles/"

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
file <- 1 # 31
rasterMap <- raster(rasters[file])
names(rasterMap)
# rasterMap[rasterMap == 0.2] <- NA

## --------------

# https://github.com/uber/h3/blob/master/docs/core-library/restable.md
resolutionH3 <- 4

rasterMapDF <- data.frame(xyFromCell(rasterMap, Which( !is.na(rasterMap) , cells=T)),val=rasterMap[Which( !is.na(rasterMap) , cells=T)])
rasterMapDF <- data.frame(rasterMapDF,hex=apply(rasterMapDF[,1:2],1,function(x) { getIndexFromCoords(x[[2]], x[[1]], resolution = resolutionH3) } ))
rasterMapDF <- data.frame(hex=unique(rasterMapDF$hex),val=sapply(unique(rasterMapDF$hex),function(x) { mean(rasterMapDF[rasterMapDF$hex == x , "val"]) } ))
rasterMapDF.polygons <- h3_to_geo_boundary_sf(rasterMapDF$hex)
rasterMapDF.polygons$hex <- rasterMapDF$hex
rasterMapDF.polygons$value <- rasterMapDF$val

max(rasterMapDF$val)

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------

fieldLabel <- "Species richness\n[number]"
fieldLabel <- "Species gain\n[number]"

## -----------------
## As Sf

plot1 <- mainGlobalMap +
         geom_sf(data = rasterMapDF.polygons, aes(fill = value) , size=0.1) +
         scale_fill_viridis_c(option = 'magma', direction = -1, begin = 0, end = 1) +
         coord_sf(crs = worldMapCoordRef) + labs(fill = fieldLabel) +
         theme(legend.position=c(.95, .95),
               legend.justification=c("right", "top"),
               legend.margin=margin(0,0,0,0),
               legend.box.margin=margin(0,0,0,-80))

plot1

## -----------------
# As Sp 

rasterMapDF.polygons <- sf:::as_Spatial(rasterMapDF.polygons)
rasterMapDF.polygons$value <- as.numeric(as.character(rasterMapDF.polygons$value))
rasterMapDF.polygons$hex <- as.character(rasterMapDF.polygons$hex)

rasterMapDF.polygons <- fortify(rasterMapDF.polygons)
rasterMapDF.polygons <- rasterMapDF.polygons[rasterMapDF.polygons$lat >= extent(worldMap)[3],]
rasterMapDF.polygons$value <- sapply(as.numeric(rasterMapDF.polygons$id), function(x) { rasterMapDF[x,"val"] })

plot1 <- mainGlobalMap +
  geom_polygon(data = rasterMapDF.polygons, aes(x = long, y = lat,group=id, fill=value), colour = NA, size = 0.1) +
  scale_fill_viridis_c(name= fieldLabel, option = 'magma', direction = -1, begin = 0, end = 1, limits=c(0,100)) +
  theme(legend.position=c(.99, .99),
        legend.justification=c("right", "top"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-80),legend.title=element_blank(),legend.background = element_rect(fill = "#ffffff", color = NA))

plot1

# + theme(legend.position = "none")

## -----------------
## -----------------

pdf(file=paste0(mainDirectory,"/"),width=12,height=12,useDingbats=FALSE)
plot1
dev.off()

# ----------------------

plot1.i <- plot1 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot2.i <- plot2 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))

# ----------------------

plotCombined <- grid.arrange(plot1.i, plot2.i, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined) + theme(plot.background = element_rect(fill="#F3F3F3", color = NA))
plotCombined

pdf(file=paste0("../Results/_ Ensembles/","/KelpsFig5.pdf"),width=12,useDingbats=FALSE)
plotCombined
dev.off()

