## --------------------------------------------------
## --------------------------------------------------
##
## Pretty maps [R]
## Muffins 'n' Code
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

## https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)

source("mainFunctions.R")

## -------------------

name <- "Ice_LGMMin"
r.1 <- "/Users/jorgeassis/Dropbox/Manuscripts/The Modelling Factory/Climate Data/LGM/Sea.ice.thickness.Surface.Var.Lt.Min.tif "

cExtent <- extent(c(90,175,0,75))

## -------------------

r.1 <- raster(r.1)
r.1 <- crop(r.1,cExtent)
r.1[r.1 <= 0.01] <- NA
r.1[r.1 >= 0.01] <- 1
plot(r.1)

r.1.poly <- rasterToPolygons(r.1, fun=NULL, n=8, na.rm=TRUE, digits=8, dissolve=TRUE)
r.1.poly
writeOGR(obj=r.1.poly, dsn="~/Dropbox/Data/Shapefiles/Global Ice", layer=name, driver="ESRI Shapefile")






