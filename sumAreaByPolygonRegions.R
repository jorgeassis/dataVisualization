
theme_map <- 
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", color = "#22211d"),
    #axis.line = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    legend.position = c(1,1),
    legend.justification=c(1, 1),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F3F3F3", color = NA), 
    panel.background = element_rect(fill = "#F3F3F3", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    plot.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)) ,
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )

library(ggplot2)
library(ggridges)

rasters <- list.files(mainDirectory,full.names = TRUE,pattern="tif")
rasters

file <- 72 # 30 32,34,36 9
rasterMap1 <- raster(rasters[file])
rasterMap1[rasterMap1 == 1] <- NA
names(rasterMap1)

file <- 73 #  32,34,36 9
rasterMap2 <- raster(rasters[file])
rasterMap2[rasterMap2 == 1] <- NA
names(rasterMap2)

file <- 74 #  32,34,36 9
rasterMap3 <- raster(rasters[file])
rasterMap3[rasterMap3 == 1] <- NA
names(rasterMap3)

file <- 75 # 30 ,34,36 9
rasterMap4 <- raster(rasters[file])
rasterMap4[rasterMap4 == 1] <- NA
names(rasterMap4)

file <- 76 # 30 ,34,36 9
rasterMap5 <- raster(rasters[file])
rasterMap5[rasterMap5 == 1] <- NA
names(rasterMap5)

area <- raster::area(rasterMap1)

# --------------

bins <- seq(50,90,by=0.5)
resultBinDiversity <- data.frame(bins,Present=NA,RCP26=NA,RCP45=NA,RCP60=NA,RCP85=NA)
resultBinArea <- data.frame(bins,Present=NA,RCP26=NA,RCP45=NA,RCP60=NA,RCP85=NA)

for(bin in bins) {
  
  row = rowFromY(rasterMap1, bin)
  cells = cellFromRow(rasterMap1, row)
  
  resultBinDiversity[resultBinDiversity$bins == bin,2] <- mean(rasterMap1[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,3] <- mean(rasterMap2[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,4] <- mean(rasterMap3[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,5] <- mean(rasterMap4[cells]  , na.rm=T)
  resultBinDiversity[resultBinDiversity$bins == bin,6] <- mean(rasterMap5[cells]  , na.rm=T)
  
  r1 <- rasterMap1[cells]; r1[!is.na(r1)] <- 1
  r2 <- rasterMap2[cells]; r2[!is.na(r2)] <- 1
  r3 <- rasterMap3[cells]; r3[!is.na(r3)] <- 1
  r4 <- rasterMap4[cells]; r4[!is.na(r4)] <- 1
  r5 <- rasterMap5[cells]; r5[!is.na(r5)] <- 1
  
  resultBinArea[resultBinArea$bins == bin,2] <- sum(r1 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,3] <- sum(r2 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,4] <- sum(r3 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,5] <- sum(r4 * area[cells] , na.rm=T)
  resultBinArea[resultBinArea$bins == bin,6] <- sum(r5 * area[cells] , na.rm=T)
  
}

resultBinArea[which(resultBinArea$RCP26 > 11413),3] <- 9785.5089
resultBinArea[,-1] <- resultBinArea[,-1] / 1000

plot1 <- ggplot() +
  geom_bar( data=resultBinArea, aes(x=bins, y=RCP60, fill="#5D1877"), stat="identity",color="black", alpha=1,size=0.1) +
  geom_bar( data=resultBinArea, aes(x=bins, y=RCP26, fill="#E7626A"), stat="identity",color="black", alpha=1,size=0.1) +
  geom_bar( data=resultBinArea, aes(x=bins, y=Present, fill="#F6F1D4"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Change in habitat suitability area for Arctic kelp forests") + 
  theme_map +
  ylab("Total habitat area (x1000 km2)") + xlab("Latitude") + 
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('RCP60','RCP26','Present')) + theme(legend.position="None")

plot2 <- ggplot() +
  geom_bar( data=resultBinDiversity, aes(x=bins, y=RCP60, fill="#5D1877"), stat="identity",color="black", alpha=1,size=0.1) +
  geom_bar( data=resultBinDiversity, aes(x=bins, y=RCP26, fill="#E7626A"), stat="identity",color="black", alpha=1,size=0.1) +
  geom_bar( data=resultBinDiversity, aes(x=bins, y=Present, fill="#F6F1D4"), stat="identity",color="black", alpha=1,size=0.1) +
  ggtitle("Change in habitat suitability area for Arctic kelp forests") + 
  theme_map +
  ylab("Average number of species with suitable habitats") + xlab("Latitude") +
  scale_fill_identity(name = '', guide = guide_legend(),labels = c('RCP60','RCP26','Present'))

plotCombined <- grid.arrange(plot1, plot2, nrow = 1)
plotCombined <- cowplot::ggdraw(plotCombined)
plotCombined

pdf(file=paste0("../../Estimating future distributional shifts of Arctic marine macroalgae/Figures/KelpsFig2.pdf"),width=20,height=8,useDingbats=FALSE)
plotCombined
dev.off()

# ----------------------------

caffBoundary <- "../../../Data/Shapefiles/CAFF Boundary/CAFF_Boundary_Polygon_4326.shp"
caffBoundary <- shapefile(caffBoundary)

finalEnsembleBaseline <- rasterMap1
finalEnsembleRCP26 <- rasterMap2
finalEnsembleRCP45 <- rasterMap3
finalEnsembleRCP60 <- rasterMap4
finalEnsembleRCP85 <- rasterMap5

max(getValues(mask(finalEnsembleBaseline,caffBoundary)),na.rm=T)
max(getValues(mask(finalEnsembleRCP26,caffBoundary)),na.rm=T)
max(getValues(mask(finalEnsembleRCP60,caffBoundary)),na.rm=T)

(48-30)/8
(56-30)/8

finalEnsembleBaseline[finalEnsembleBaseline >= 1] <- 1
finalEnsembleRCP26[finalEnsembleRCP26 >= 1] <- 1
finalEnsembleRCP45[finalEnsembleRCP45 >= 1] <- 1
finalEnsembleRCP60[finalEnsembleRCP60 >= 1] <- 1
finalEnsembleRCP85[finalEnsembleRCP85 >= 1] <- 1

sum(getValues(mask(raster::area(finalEnsembleBaseline),caffBoundary) * mask(finalEnsembleBaseline,caffBoundary)),na.rm=TRUE) # km2

sum(getValues(mask(raster::area(finalEnsembleRCP26),caffBoundary) * mask(finalEnsembleRCP26,caffBoundary)),na.rm=TRUE) # km2
sum(getValues(mask(raster::area(finalEnsembleRCP45),caffBoundary) * mask(finalEnsembleRCP45,caffBoundary)),na.rm=TRUE) # km2
sum(getValues(mask(raster::area(finalEnsembleRCP60),caffBoundary) * mask(finalEnsembleRCP60,caffBoundary)),na.rm=TRUE) # km2
sum(getValues(mask(raster::area(finalEnsembleRCP85),caffBoundary) * mask(finalEnsembleRCP85,caffBoundary)),na.rm=TRUE) # km2

# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# Polar migration km decade-1

finalEnsembleBaselineDF <- as.data.frame(finalEnsembleBaseline,xy=TRUE)
finalEnsembleBaselineDF <- finalEnsembleBaselineDF[!is.na(finalEnsembleBaselineDF[,3]),]
max(finalEnsembleBaselineDF$y)

finalEnsembleRCP26DF <- as.data.frame(finalEnsembleRCP26,xy=TRUE)
finalEnsembleRCP26DF <- finalEnsembleRCP26DF[!is.na(finalEnsembleRCP26DF[,3]),]
max(finalEnsembleRCP26DF$y)

finalEnsembleRCP60DF <- as.data.frame(finalEnsembleRCP60,xy=TRUE)
finalEnsembleRCP60DF <- finalEnsembleRCP60DF[!is.na(finalEnsembleRCP60DF[,3]),]
max(finalEnsembleRCP60DF$y)

(((83.79167 - 81.125) / 8)) * 111
(((83.70833 - 81.125) / 8)) * 111


# ----------------------------
# Other Arctic regions

caffBoundary <- "../../../Data/Shapefiles/CAFF Boundary/CAFF_Boundary_Polygon_4326.shp"
caffBoundary <- shapefile(caffBoundary)

regions <- "../../Estimating future distributional shifts of Arctic marine macroalgae/Data/Old/regions.shp"
regions <- shapefile(regions)

finalEnsembleBaseline <- rasterMap1
finalEnsembleRCP26 <- rasterMap2
finalEnsembleRCP45 <- rasterMap3
finalEnsembleRCP60 <- rasterMap4
finalEnsembleRCP85 <- rasterMap5

rregionPresent <- mask(finalEnsembleBaseline,caffBoundary)
rregionRCP26 <- mask(finalEnsembleRCP26,caffBoundary) 
rregionRCP60 <- mask(finalEnsembleRCP60,caffBoundary)

rregionSSTMaxPresent <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Present/Surface/LongTerm/OceanTemperature Surface Pred Max.tif")
rregionSSTMaxRCP26 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP26/Surface/OceanTemperature Surface Pred Max.tif")
rregionSSTMaxRCP26 <- mask(rregionSSTMaxRCP26 - rregionSSTMaxPresent,caffBoundary)
rregionSSTMaxRCP60 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP60/Surface/OceanTemperature Surface Pred Max.tif")
rregionSSTMaxRCP60 <- mask(rregionSSTMaxRCP60 - rregionSSTMaxPresent,caffBoundary)

rregionIceMaxPresent <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Present/Surface/LongTerm/SeaIceThickness Surface Pred LtMax.tif")
rregionIceMaxRCP26 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP26/Surface/SeaIceThickness Surface Pred LtMax.tif")
rregionIceMaxRCP26 <- mask(rregionIceMaxRCP26 - rregionIceMaxPresent,caffBoundary)
rregionIceMaxRCP60 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP60/Surface/SeaIceThickness Surface Pred LtMax.tif")
rregionIceMaxRCP60 <- mask(rregionIceMaxRCP60 - rregionIceMaxPresent,caffBoundary)

rregionIceMinPresent <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Present/Surface/LongTerm/SeaIceThickness Surface Pred LtMin.tif")
rregionIceMinRCP26 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP26/Surface/SeaIceThickness Surface Pred LtMin.tif")
rregionIceMinRCP26 <- mask(rregionIceMinRCP26 - rregionIceMinPresent,caffBoundary)
rregionIceMinRCP60 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP60/Surface/SeaIceThickness Surface Pred LtMin.tif")
rregionIceMinRCP60 <- mask(rregionIceMinRCP60 - rregionIceMinPresent,caffBoundary)

resultsRegion <- data.frame(region=1:nrow(regions),DivPresent=NA,DivRCP26=NA,DivRCP60=NA,AreaPresent=NA,AreaRCP26=NA,AreaRCP60=NA,WarmingRCP26=NA,WarmingRCP60=NA,IceMinRCP26=NA,IceMinRCP60=NA,IceMaxRCP26=NA,IceMaxRCP60=NA)

for( i in 1:nrow(regions)) {
  
  resultsRegion[i,"DivPresent"] <- max(getValues(mask(rregionPresent,regions[i,])),na.rm=T)
  resultsRegion[i,"DivRCP26"] <- max(getValues(mask(rregionRCP26,regions[i,])),na.rm=T) 
  resultsRegion[i,"DivRCP60"] <- max(getValues(mask(rregionRCP60,regions[i,])),na.rm=T) 
  
  r1 <- mask(rregionPresent,regions[i,])
  r2 <- mask(rregionRCP26,regions[i,])
  r3 <- mask(rregionRCP60,regions[i,])
  
  r1[r1 >= 1] <- 1
  r2[r2 >= 1] <- 1
  r3[r3 >= 1] <- 1
  
  resultsRegion[i,"region"] <- regions[i,]$id
  
  resultsRegion[i,"AreaPresent"] <- sum(getValues(raster::area(r1) * r1),na.rm=T)
  resultsRegion[i,"AreaRCP26"] <- sum(getValues(raster::area(r2) * r2),na.rm=T)
  resultsRegion[i,"AreaRCP60"] <- sum(getValues(raster::area(r2) * r3),na.rm=T)
  
  resultsRegion[i,"WarmingRCP26"] <- cellStats(mask(rregionSSTMaxRCP26,regions[i,]),max)
  resultsRegion[i,"WarmingRCP60"] <- cellStats(mask(rregionSSTMaxRCP60,regions[i,]),max)
  
  resultsRegion[i,"IceMinRCP26"] <- cellStats(mask(rregionIceMinRCP26,regions[i,]),min)
  resultsRegion[i,"IceMinRCP60"] <- cellStats(mask(rregionIceMinRCP60,regions[i,]),min)
  resultsRegion[i,"IceMaxRCP26"] <- cellStats(mask(rregionIceMaxRCP26,regions[i,]),min)
  resultsRegion[i,"IceMaxRCP60"] <- cellStats(mask(rregionIceMaxRCP60,regions[i,]),min)
  
  
}

rregionIceMinPresent <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Present/Surface/LongTerm/SeaIceThickness Surface Pred LtMin.tif")
rregionIceMinRCP26 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP26/Surface/SeaIceThickness Surface Pred LtMin.tif")
rregionIceMinRCP60 <- raster("/Volumes/Jellyfish/GDrive/Manuscripts/Bio-ORACLE Across Climate Changes/Bioclimatic Layers BO3.0/Future 2100/RCP60/Surface/SeaIceThickness Surface Pred LtMin.tif")

resultsRegion2 <- data.frame(region=1:nrow(regions),IceMinPresent=NA,IceMinRCP26=NA,IceMinRCP60=NA,IceMaxPresent=NA,IceMaxRCP26=NA,IceMaxRCP60=NA)

for( i in 1:nrow(regions)) {
  
  r1 <- mask(rregionIceMinPresent,regions[i,])
  r2 <- mask(rregionIceMinRCP26,regions[i,])
  r3 <- mask(rregionIceMinRCP60,regions[i,])
  
  r1[r1 >= 1] <- 1
  r2[r2 >= 1] <- 1
  r3[r3 >= 1] <- 1
  
  resultsRegion2[i,"region"] <- regions[i,]$id
  
  resultsRegion2[i,"IceMinPresent"] <- sum(getValues(raster::area(r1) * r1),na.rm=T)
  resultsRegion2[i,"IceMinRCP26"] <- sum(getValues(raster::area(r2) * r2),na.rm=T)
  resultsRegion2[i,"IceMinRCP60"] <- sum(getValues(raster::area(r3) * r3),na.rm=T)
  
  r1 <- mask(rregionIceMaxPresent,regions[i,])
  r2 <- mask(rregionIceMaxRCP26,regions[i,])
  r3 <- mask(rregionIceMaxRCP60,regions[i,])
  
  r1[r1 >= 1] <- 1
  r2[r2 >= 1] <- 1
  r3[r3 >= 1] <- 1
  
  resultsRegion2[i,"IceMaxPresent"] <- sum(getValues(raster::area(r1) * r1),na.rm=T)
  resultsRegion2[i,"IceMaxRCP26"] <- sum(getValues(raster::area(r2) * r2),na.rm=T)
  resultsRegion2[i,"IceMaxRCP60"] <- sum(getValues(raster::area(r3) * r3),na.rm=T)
  
}


