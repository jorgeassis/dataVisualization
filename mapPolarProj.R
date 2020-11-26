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

library(ggplot2)
library(rnaturalearth)
library(raster)
library(rgeos)

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Files and confies

cExtent <- c(-180,180,45,90)
worldMap <- ne_countries(scale = 10, returnclass = "sp")
caffBoundary <- "/Volumes/Jellyfish/Dropbox/Data/Shapefiles/CAFF Boundary/CAFF_Boundary_Line_4326.shp"

iceMapMin <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers/Historical 1950/Surface/SeaIceCover Surface Pred LtMin.tif"
iceMapMax <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers/Historical 1950/Surface/SeaIceCover Surface Pred LtMax.tif"

iceMapMin <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers/Present/Surface/LongTerm/SeaIceCover Surface Pred LtMin.tif"
iceMapMax <- "/Volumes/Jellyfish/Dropbox/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers/Present/Surface/LongTerm/SeaIceCover Surface Pred LtMax.tif"

## -----------------

iceMapMin <- crop(raster(iceMapMin),cExtent)
iceMapMax <- crop(raster(iceMapMax),cExtent)

iceMapMin[iceMapMin > 0] <- 1 ; iceMapMin[iceMapMin == 0] <- NA
iceMapMax[iceMapMax > 0] <- 1 ; iceMapMax[iceMapMax == 0] <- NA

iceMapMin <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(iceMapMin), as_points = FALSE, merge = TRUE) )
iceMapMax <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(iceMapMax), as_points = FALSE, merge = TRUE) )

iceMapMin <- crop(gBuffer(iceMapMin, byid=TRUE, width=0),cExtent)
iceMapMax <- crop(gBuffer(iceMapMax, byid=TRUE, width=0),cExtent)

if(class(worldMap) == "character") { worldMap <- shapefile(worldMap)}
worldMap <- crop(worldMap,cExtent)
worldMap <- aggregate(worldMap,dissolve=T)
worldMap <- gSimplify(worldMap, tol = 0.05, topologyPreserve = TRUE)

caffBoundary <- shapefile(caffBoundary)

## -----------------
## -----------------
# polar map

x_lines <- seq(-120,180, by = 60)

icePast <- ggplot() +
  
  geom_polygon(data = iceMapMax, aes(x = long, y = lat, group = group), fill="#BCD9EC", colour = NA) +
  geom_path(data = iceMapMax, aes(x = long, y = lat, group = group), color = "#BCD9EC", size = 0.1) +
  
  geom_polygon(data = iceMapMin, aes( x = long, y = lat, group = group), fill="#89B2C7", colour = NA) + #89B2C7
  geom_path(data = iceMapMin, aes(x = long, y = lat, group = group), color = "#89B2C7", size = 0.1) +
  
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#E0DAD5", colour = NA) +
 
  theme(legend.position = "none") +
  theme(text = element_text(family = "Helvetica", color = "#22211d")) +
  theme(panel.background = element_blank(), axis.ticks=element_blank()) +

  coord_map("ortho", orientation = c(90, 0, 0)) +
  scale_y_continuous(breaks = seq(45, 90, by = 5), labels = NULL) +
  
  scale_x_continuous(breaks = NULL) + xlab("") +  ylab("") +
  
  # Adds labels
  geom_text(size=3.5 , aes(x = 180, y = seq(53.3, 83.3, by = 15), hjust = -0.3, label = paste0(seq(55, 85, by = 15), "°N"))) +
  geom_text(size=3.5 , aes(x = x_lines, y = (41 + c(-3,-3,0,-3,-3,0)), label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
  
  # Adds axes
  geom_hline(aes(yintercept = 45), size = 0.5, colour = "gray")  +
  geom_segment(size = 0.1,aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
  
  geom_segment(size = 1.2 ,aes(y = 45, yend = 45, x = -180, xend = 0), colour = "gray") +
  geom_segment(size = 1.2 ,aes(y = 45, yend = 45, x = 180, xend = 0), colour = "gray") +
  
  geom_segment(size = 0.1 ,aes(y = 55, yend = 55, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 55, yend = 55, x = 180, xend = 0), linetype = "dashed") +
  
  geom_segment(size = 0.1 ,aes(y = 70, yend = 70, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 70, yend = 70, x = 180, xend = 0), linetype = "dashed") +
  
  geom_segment(size = 0.1 ,aes(y = 85, yend = 85, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 85, yend = 85, x = 180, xend = 0), linetype = "dashed") +
  
  geom_path(data = caffBoundary, aes(x = long, y = lat, group = group), color = "#414141", size = 0.2)
  
# ------------------------

dev.off(); gc() # 8:8
icePresent
icePast

# ------------------------

rasterMap <- "Results/Intertidal meanIcean/lossGain.1950.tif"
rasterMap <- "Results/Intertidal meanIcean/EnsembleReclass.1950.tif"

rasterMap <- "Results/Benthic/lossGain.1950.mask.tif"
rasterMap <- "Results/Benthic/EnsembleReclass.1950.mask.tif"

rasterMap <- crop( raster(rasterMap), cExtent )
rasterMap <- aggregate(rasterMap,2, fun=min)

rasterMap <- as.data.frame(rasterMap,xy=T)
rasterMap <- rasterMap[ !is.na(rasterMap[,3]),]
rasterMap <- rasterMap[ rasterMap[,3] != 0,]
colnames(rasterMap) <- c("Lon","Lat","value")

rasterMap <- rasterMap[ rasterMap[,3] != 2,]

x_lines <- seq(-120,180, by = 60)

distributionChangeSub <- ggplot() +

  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#E0DAD5", colour = NA) +

  geom_tile(data = rasterMap , aes(x = Lon, y = Lat, fill = cut(value, breaks=0:3, labels=1:3))) +
  scale_fill_manual(drop=FALSE, values=c("#4B9755", "#AC1322", "#234776"), labels=c("Gain","Loss","Stable"), na.value="transparent", name="Future range") +
  
  #geom_tile(data = rasterMap , aes(Lon, Lat, fill = value)) +
  #scale_fill_gradientn(colours="#234776", na.value='transparent',limits=c(1,1)) +

  theme(legend.position = "none") +
  theme(text = element_text(family = "Helvetica", color = "#22211d")) +
  
  coord_map("ortho", orientation = c(90, 0, 0)) +
  scale_y_continuous(breaks = seq(45, 90, by = 5), labels = NULL) +
  
  scale_x_continuous(breaks = NULL) + xlab("") +  ylab("") +
  
  geom_text(size=3.5 , aes(x = 180, y = seq(53.3, 83.3, by = 15), hjust = -0.3, label = paste0(seq(55, 85, by = 15), "°N"))) +
  geom_text(size=3.5 , aes(x = x_lines, y = (41 + c(-3,-3,0,-3,-3,0)), label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
  
  geom_hline(aes(yintercept = 45), size = 0.5, colour = "gray")  +
  geom_segment(size = 0.1,aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
  
  geom_segment(size = 1.2 ,aes(y = 45, yend = 45, x = -180, xend = 0), colour = "gray") +
  geom_segment(size = 1.2 ,aes(y = 45, yend = 45, x = 180, xend = 0), colour = "gray") +
  
  geom_segment(size = 0.1 ,aes(y = 55, yend = 55, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 55, yend = 55, x = 180, xend = 0), linetype = "dashed") +

  geom_segment(size = 0.1 ,aes(y = 70, yend = 70, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 70, yend = 70, x = 180, xend = 0), linetype = "dashed") +
  
  geom_segment(size = 0.1 ,aes(y = 85, yend = 85, x = -180, xend = 0), linetype = "dashed") +
  geom_segment(size = 0.1 ,aes(y = 85, yend = 85, x = 180, xend = 0), linetype = "dashed") +
  
  geom_path(data = caffBoundary, aes(x = long, y = lat, group = group), color = "#414141", size = 0.2) +
  
  # Change theme to remove axes and ticks
  theme(panel.background = element_blank(), axis.ticks=element_blank())

dev.off(); gc() # 8:8
distributionPast
distributionChanges
distributionPastSub
distributionChangeSub
icePresent
icePast

library(gridExtra)
grid.arrange(icePast, distributionPast,distributionPastSub, ncol=3) # 2: 12/8  3: 14/8

library(gridExtra)
grid.arrange(icePresent, distributionChanges,distributionChangeSub, ncol=3) # 2: 12/8  3: 14/8

# Create plot with legend

plotLegend <- plotLGM + theme(legend.position = "bottom", legend.background = element_rect(fill = "#ffffff", color = NA))

# Create user-defined function, which extracts legends from ggplots
extractLegend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
sharedLegend <- extractLegend(plotLegend)

plotP2 <- plotP + theme( plot.background = element_rect(fill = "#ffffff", color = NA) , panel.background = element_rect(fill = "#ffffff", color = NA)  )
plotMH2 <- plotMH + theme( plot.background = element_rect(fill = "#ffffff", color = NA) , panel.background = element_rect(fill = "#ffffff", color = NA) )
plotLGM2 <- plotLGM + theme( plot.background = element_rect(fill = "#ffffff", color = NA) , panel.background = element_rect(fill = "#ffffff", color = NA) )

grid.arrange( arrangeGrob(plotLGM2, plotMH2, plotP2, ncol = 3), sharedLegend, nrow = 2, heights = c(10, 1)) # 15:5
grid.arrange( arrangeGrob(plotRCP26, plotRCP85, ncol = 2), sharedLegend, nrow = 2, heights = c(10, 1))

grid.arrange(plotP, plotMH, plotLGM, ncol=3) # 3: 14/8
grid.arrange(plot1, plot2, ncol=2) # 3: 10/8


## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code