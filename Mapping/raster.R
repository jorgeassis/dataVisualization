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

## --------------------------------------------------
## --------------------------------------------------
## Custom Files, Extent and Colors

cExtent <- extent(c(10,30,-38,-12))

cColors <- c("#8EB0D6", "#F8F4C1","#E29939","#AA1313")
cColorsGray <- c("#868686", "#000000")
panelColor <- "#ffffff"
oceanColor <- "#ffffff"

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------
## Raster map

worldMap <- ne_countries(scale = 10, returnclass = "sp")
oceanMap <- "Data/GRAY_HR_SR_OB.tif"
rasterMap <- "Data/KelpDiversity.tif"

worldMap <- crop( worldMap, cExtent )

oceanMap <- crop( raster(oceanMap), cExtent )
oceanMap <- as.data.frame(oceanMap,xy=TRUE,na.rm=T)
colnames(oceanMap) <- c("Lon","Lat","Val")

rasterMap <- crop( raster(rasterMap), cExtent )
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

captionMap <- "My caption"
myColors <- c("#6FBBE8","#A1ECD8","#F6F9AB","#FCB46D","#B21414")

plot1 <- ggplot() +
  
  # geom_tile(data = oceanMap, aes(x=Lon,y=Lat,fill=Val)) +
  # scale_fill_gradientn(colours=cColorsGray, limits=c(69,150)) + # Continuous
  
  new_scale("fill") +
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), fill="#D2D2D2", colour = "#D2D2D2" , size=0.25 ) +
  
  geom_tile(data = rasterMap, aes(x=Lon,y=Lat,fill=Val)) +
  scale_fill_gradientn(colours=myColors, na.value='transparent') + # ,limits=c(1,1) BINOMIAL

  # geom_segment(colour = "Black" , size = 0.15,aes(y = cExtent[3]-1, yend = cExtent[4]+1, x = seq(cExtent[1],cExtent[2], by = 5)[2:4], xend = seq(cExtent[1],cExtent[2], by = 5)[2:4]), linetype = "dashed") +
  # geom_segment(colour = "Black" , size = 0.15,aes(y = seq(cExtent[3],cExtent[4], by = 5)[2:5], yend =seq(cExtent[3],cExtent[4], by = 5)[2:5], x = cExtent[1]-1, xend = cExtent[2]+1), linetype = "dashed") +
  
  coord_equal() + # coord_map('lambert', lat0=extent(worldMap)[4], lat1=extent(worldMap)[3]) + 
  theme_map + 
  theme(legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.75, 'cm'),
        legend.background = element_rect(fill = "#F3F3F3", color = NA)) + theme(legend.title=element_blank()) +
  annotate(geom="text", x=cExtent[2]-cExtent[1], y=cExtent[3]-2, label=captionMap,size=6,family="Helvetica", color = "#7F7F7F",hjust = 0.5)

plot1

# --------------------
# Multiple Plot

pdf(file="Plot.pdf",width=15,useDingbats=FALSE)
grid.arrange(plot1,plot2,plot3, ncol = 3)
dev.off()

## ---------------------------------------------------------------------------
## ---------------------------------------------------------------------------
## End of code