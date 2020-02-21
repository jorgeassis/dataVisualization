

# Spherical Geodesic Grids

library(dggridR)
library(dplyr)
library(rnaturalearth)
library(maptools)

world <- ne_countries(scale = 'medium')

#Construct a discrete global grid (geodesic) with cells of km^2
dggs <- dgconstruct(area=250000, metric=FALSE, resround='nearest')
dgmaxcell(dggs)
dggs <- dgearthgrid(dggs,frame=FALSE, wrapcells = TRUE)

mean( area(dggs) / 10000000 ) # km2

ggplot() + 
  geom_polygon(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4) +
  geom_path(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(151, 150, 90))

#Construct a discrete global grid (geodesic) with cells of km wide
dggs <- dgconstruct(spacing=1000, metric=TRUE, resround='nearest', projection = "ISEA") # ISEA 
dgmaxcell(dggs)
dgverify(dggs)

dggs <- dgearthgrid(dggs,frame=FALSE, wrapcells = TRUE)
# dggs <- dgcellstogrid(dggs,1:dgmaxcell(dggs),frame=FALSE,wrapcells=TRUE)

mean( area(dggs) / 10000000 ) # km2

dggs$id <- 1:length(dggs)

ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4) +
  geom_path(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(0, 120, 0))


CoastLine <- shapefile("/Volumes/Jellyfish/Dropbox/Data/Shapefiles/CoastLine Polyline LR.shp")

library(rgeos)
dggs <- gBuffer(dggs, byid=TRUE, width=0)

clip2 <- gIntersection(Polygone1, Polygone2, byid=TRUE)

dggsCoast <- intersect(CoastLine, dggs)
plot(dggsCoast)


#Construct a discrete global grid (geodesic) with cells of km wide
dggs <- dgconstruct(spacing=500, metric=TRUE, resround='nearest', projection = "ISEA") # ISEA 
dggs <- dgshptogrid(dggs,"/Volumes/Jellyfish/Dropbox/Data/Shapefiles/CoastLine Polyline LR.shp",frame=FALSE,wrapcells=TRUE)
class(dggs)

ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group), color="black")   +
  geom_polygon(data=dggs,      aes(x=long, y=lat, group=group), alpha=0.4)    +
  geom_path   (data=dggs,      aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(0, 120, 0))




mean( area(dggs) / 10000000 ) # km2

dggs$id <- 1:length(dggs)

ggplot() + 
  geom_polygon(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4) +
  geom_path(data=dggs, aes(x=long, y=lat, group=group), alpha=0.4, color="white") +
  coord_map("ortho", orientation = c(151, 150, 90))
