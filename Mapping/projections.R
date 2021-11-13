## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

# Projections and mapped projections

library(ggplot2)
library(rnaturalearth)
library(sf)
library(rgdal)
library(raster)
library(geosphere)

## --------------------------------------------------

# projecting vector data
crs(mpa) <- CRS("+proj=longlat +datum=WGS84")
crs(mpa)

# projecting vector data :: Mollweide projection (cells have equal-area, generally used for global maps)
proj <- "+proj=moll"
worldProjected <- spTransform(world, CRSobj = CRS(proj))

# projecting vector data :: Universal Transverse Mercator projected coordinate system
portugal <- spTransform(portugal, CRS("+proj=utm +zone=29 +datum=WGS84"))

# projecting raster data
proj <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
maxSSTPolar <- projectRaster(maxSSTPolar, crs=CRS(proj))

## --------------------------------------------------

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

## ------------------------
# Robinson equal area projection
# https://bluegreenlabs.org/post/map-building-3/

projection <- CRS("+proj=robin +over")

bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)), n = 100))
bb <- st_transform(bb, "+proj=robin +over")

worldMap <- ne_countries(scale = 10, returnclass = "sf")
worldMap <- st_transform(worldMap, projection)

rasterMap <- raster("Data/KelpDiversity.tif")
e <- as(extent(rasterMap), "SpatialPolygons")
crs(e) <- crs(rasterMap)
e <- makePoly(e)  # add additional vertices
re <- spTransform(e, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m")
rasterMap <- projectRaster(rasterMap, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m")
rasterMap <- mask(rasterMap, re)
rasterMap <- as.data.frame(rasterMap,xy=TRUE,na.rm=T)
colnames(rasterMap) <- c("Lon","Lat","Val")

myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414")

mainGlobalMap <- ggplot() + 
 
  geom_raster(data = rasterMap, aes(x=Lon,y=Lat,fill=Val), interpolate = FALSE) + 
  scale_fill_gradientn(name= "AE", colours=myColors, na.value='transparent') + # ,limits=c(
  
  geom_sf(data = worldMap,fill="#c4c4c4", colour = "#B6B6B6" , size=0.1 ) +
  geom_sf(data = bb,fill=NA, colour = "grey25" , linetype='solid', size=0.2 ) +
  theme(axis.ticks=element_blank()) + ylab("") + xlab("") +
  theme_map +
  theme(legend.position="bottom",
        legend.margin=margin(-15,0,0,0),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.75, 'cm'),
        legend.background = element_rect(fill = "#FFFFFF", color = NA)) + theme(legend.title=element_blank())

mainGlobalMap
