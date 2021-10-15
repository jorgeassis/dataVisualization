## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

library(ggnewscale)
library(h3r)
library(h3)
library(sf)

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

## ------------

# https://github.com/uber/h3/blob/master/docs/core-library/restable.md
resolutionH3 <- 3
rasterMapDF <- data.frame(xyFromCell(rasterMap, Which( !is.na(rasterMap) , cells=T)),val=rasterMap[Which( !is.na(rasterMap) , cells=T)])
rasterMapDF <- data.frame(rasterMapDF,hex=apply(rasterMapDF[,1:2],1,function(x) { h3js::h3_geo_to_h3(x[[2]], x[[1]], res = resolutionH3) } ))
rasterMapDF <- data.frame(hex=unique(rasterMapDF$hex),val=sapply(unique(rasterMapDF$hex),function(x) { max(rasterMapDF[rasterMapDF$hex == x , "val"],na.rm=T) } ))
rasterMapDF.polygons <- h3jsr::h3_to_polygon(rasterMapDF$hex)
rasterMapDF.polygons <- st_sf(rasterMapDF.polygons)
rasterMapDF.polygons$hex <- as.character(rasterMapDF$hex)
rasterMapDF.polygons$value <- as.numeric(as.character(rasterMapDF$val))
rasterMapDF.polygons <- st_wrap_dateline(rasterMapDF.polygons, options = "WRAPDATELINE=YES", quiet = TRUE)
rasterMapDF.polygons <- sf:::as_Spatial(rasterMapDF.polygons)
rasterMapDF.polygons$value <- as.numeric(as.character(rasterMapDF.polygons$value))
rasterMapDF.polygons$hex <- as.character(rasterMapDF.polygons$hex)
rasterMapDF.polygons <- fortify(rasterMapDF.polygons)
rasterMapDF.polygons <- rasterMapDF.polygons[rasterMapDF.polygons$lat >= extent(worldMap)[3],]
rasterMapDF.polygons$value <- sapply(as.numeric(rasterMapDF.polygons$id), function(x) { rasterMapDF[x,"val"] })
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
