## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

library(leaflet)
library(raster)

paramuriceaClavata <- read.csv("Data/dataBases/Paramuricea_clavata.csv",sep=";",header=TRUE)
colnames(paramuriceaClavata)
m <- leaflet()
m <- addTiles(m)
m <- addCircleMarkers(m, 
                      lng=paramuriceaClavata[,"Lon"], 
                      lat=paramuriceaClavata[,"Lat"] )
m

mpa <- shapefile("Data/vectorShapefiles/noTakeMPA/MPA.shp")

m <- leaflet()
m <- addTiles(m)
m <- setView(m, lng=20, lat=35, zoom=3)
m <- addCircleMarkers(m, 
                      lng=paramuriceaClavata[,"Lon"], 
                      lat=paramuriceaClavata[,"Lat"],
                      popup= paste0("Reference: ", paramuriceaClavata[,"Main.reference"]) , 
                      radius = 3, 
                      color = "Red" , 
                      stroke = FALSE, 
                      fillOpacity = 1 )
m <- addPolygons(m,data=mpa,color = "Black", stroke = 2 , weight = 3)
m