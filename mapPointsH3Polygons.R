
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
worldMap <- ne_countries(scale = 10, returnclass = "sf")

## ------------

mapExtent = c(xmin = -35, ymin = 20, xmax = 50.05, ymax = 85)

worldMap <- st_crop(worldMap,mapExtent)
worldMap <- worldMap[worldMap$continent != "North America",]
worldMapCoordRef <- paste0("+proj=laea +lat_0=",mapExtent[4]-mapExtent[3]," +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

mainGlobalMap <- ggplot() + 
  geom_sf(data = worldMap,fill="#ABABAB", colour = "#F3F3F3" , size=0.1 ) +
  theme(axis.ticks=element_blank()) + ylab("") + xlab("") +
  theme_map + coord_sf(crs = worldMapCoordRef)
mainGlobalMap

## --------------

dataFolder <- "../Data/"
load(paste0(dataFolder,"/datasetMF.RData"))
dataset <- data.frame(dataset,stringsAsFactors = FALSE)
dataset$decimalLongitude <- as.numeric(as.character(dataset$decimalLongitude ))
dataset$decimalLatitude <- as.numeric(as.character(dataset$decimalLatitude ))

groupVal <- c("Alariaceae","Laminariaceae","Tilopteridaceae") # "Ochrophyta"
groupName <- "family" # phylum

subseter <- which( dataset[,groupName] %in% groupVal )
dataset <- dataset[subseter,]

occurrenceRecords <- dataset[dataset$decimalLongitude >= mapExtent[1] & dataset$decimalLongitude <= mapExtent[3] & dataset$decimalLatitude >= mapExtent[2] & dataset$decimalLatitude <= mapExtent[4] ,c("decimalLongitude","decimalLatitude")]
colnames(occurrenceRecords) <- c("Lon","Lat")
nrow(occurrenceRecords)

## ----------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------

captionMap <- "a) Occurrence records" 

## -----------------

occurrenceRecordsSf <- occurrenceRecords
coordinates(occurrenceRecordsSf) <- ~Lon+Lat
crs(occurrenceRecordsSf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
occurrenceRecordsSf <- st_as_sf(x = occurrenceRecordsSf,coords = c("Lon", "Lat"))
occurrenceRecordsSf <- st_transform(occurrenceRecordsSf, crs = worldMapCoordRef)

## -----------------

occurrenceRecordsSf$ID <- 1:nrow(occurrenceRecordsSf)
mask <- drawPolygon2(occurrenceRecords)
maskST <- st_as_sf(mask)
maskST <- st_transform(maskST, crs = worldMapCoordRef)
st_crs(maskST) <- worldMapCoordRef
maskST$ID <- 1
occurrenceRecordsIn <- st_join(occurrenceRecordsSf, maskST, join = st_intersects)
occurrenceRecordsIn <- occurrenceRecordsSf[which(!is.na(occurrenceRecordsIn$ID.y)),]

## -----------------

fieldLabel <- "Species richness\n[number]"

plot1 <- mainGlobalMap +
  geom_sf(data = occurrenceRecordsIn, size = 1, shape = 21, fill = "#911616" , color="#911616" , stroke = 0.15, alpha = 0.85) +
  coord_sf(crs = worldMapCoordRef)
plot1

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

