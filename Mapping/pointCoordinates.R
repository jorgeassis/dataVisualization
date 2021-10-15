
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

## ------------------------
# Global projection

mapExtent = c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)

worldMap <- ne_countries(scale = 10, returnclass = "sf")
worldMapCoordRef <- crs(worldMap)

mainGlobalMap <- ggplot() + 
  geom_sf(data = worldMap,fill="#ABABAB", colour = "#9B9B9B" , size=0.1 ) +
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
  geom_sf(data = worldMap,fill="#ABABAB", colour = "#9B9B9B" , size=0.1 ) +
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
  geom_polygon(data = worldMap,aes(x = long, y = lat, group = group),fill="#C9C9C9", colour = "#9B9B9B" , size=0.1 ) +
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

## --------------------------------------------------
## --------------------------------------------------
## Occurrence records

dataFolder <- "../Data/"
load(paste0(dataFolder,"/datasetMF.RData"))
dataset <- data.frame(dataset,stringsAsFactors = FALSE)
dataset$decimalLongitude <- as.numeric(as.character(dataset$decimalLongitude ))
dataset$decimalLatitude <- as.numeric(as.character(dataset$decimalLatitude ))

## ------------------------
## Subset by taxa

unique(dataset$family)

groupVal <- c("Alariaceae","Laminariaceae","Tilopteridaceae")
groupName <- "family" # family
subseter <- which( dataset[,groupName] %in% groupVal )
dataset <- dataset[subseter,]

## ------------------------
## Subset by region

region <- shapefile("/Volumes/Jellyfish/GDrive/Manuscripts/Projecting Arctic marine forests biodiversity/Data/cold_temperate.shp")
region$id <- 1:nrow(region)
datasetPts <- dataset
coordinates(datasetPts) <- ~decimalLongitude+decimalLatitude
crs(datasetPts) <- crs(region) 
pointsInRegion <- over(datasetPts,region)
pointsInRegion <- which(pointsInRegion == 1)
groupVal <- unique(dataset[pointsInRegion,"acceptedName"])
groupName <- "acceptedName"
subseter <- which( dataset[,groupName] %in% groupVal )
dataset <- dataset[subseter,]

## ------------------------

occurrenceRecords <- dataset[dataset$decimalLongitude >= mapExtent[1] & dataset$decimalLongitude <= mapExtent[3] & dataset$decimalLatitude >= mapExtent[2] & dataset$decimalLatitude <= mapExtent[4] ,c("decimalLongitude","decimalLatitude")]
colnames(occurrenceRecords) <- c("Lon","Lat")
nrow(occurrenceRecords)
plot(occurrenceRecords,pch=19)

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------

captionMap <- "a) Occurrence records" 

## -----------------
## As sf

coordinates(occurrenceRecords) <- ~Lon+Lat
crs(occurrenceRecords) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

occurrenceRecordsSf <- st_as_sf(x = occurrenceRecords,coords = c("Lon", "Lat"))
occurrenceRecordsSf <- st_transform(occurrenceRecordsSf, crs = worldMapCoordRef)
occurrenceRecordsSf$ID <- 1:nrow(occurrenceRecordsSf)
occurrenceRecordsIn <- occurrenceRecordsSf

mask <- drawPolygon2(occurrenceRecords)
maskST <- st_as_sf(mask)
maskST <- st_transform(maskST, crs = worldMapCoordRef)
st_crs(maskST) <- worldMapCoordRef
maskST$ID <- 1
occurrenceRecordsIn <- st_join(occurrenceRecordsSf, maskST, join = st_intersects)
occurrenceRecordsIn <- occurrenceRecordsSf[which(!is.na(occurrenceRecordsIn$ID.y)),]

fieldLabel <- "Species richness\n[number]"

plot1 <- mainGlobalMap +
  geom_sf(data = occurrenceRecordsIn, size = 1, shape = 21, fill = "#911616" , color="#911616" , stroke = 0.15, alpha = 0.85) +
  coord_sf(crs = worldMapCoordRef)
plot1

## -----------------
## As sp

plot1 <- mainGlobalMap +
  geom_point(data = occurrenceRecords, aes(x = Lon, y = Lat), colour = "#901010",size=0.25) +
  theme(legend.position=c(.99, .99),
        legend.justification=c("right", "top"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,-80),legend.title=element_blank(),legend.background = element_rect(fill = "#ffffff", color = NA))

pdf(file=paste0("../Results/_ Figures/","/KelpsGlobalFig1.pdf"),width=12,useDingbats=FALSE)
plot1
dev.off()

# ----------------------

plot1.i <- plot1 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
plot2.i <- plot2 + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))

# ----------------------

plotCombined <- grid.arrange(plot1.i, plot2.i, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined) + theme(plot.background = element_rect(fill="#F3F3F3", color = NA))
plotCombined

pdf(file=paste0(mainDirectory,"/Fig1.pdf"),width=12,useDingbats=FALSE)
plotCombined
dev.off()

