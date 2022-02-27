################################# 
##                             ## 
##  EU-Norway VMS & Logbook    ## 
##   Herring Data Extras       ## 
##                             ## 
################################# 

#- Clear workspace 
rm(list=ls()) 

library(tidyverse)   # installs sweeps of packages
library(sf)          # working with sf objects in R
library(raster)      # working with rasters in R
library(rgdal)       # links to the GDAL library
library(rasterVis)   # raster visualization
library(marmap)      # access global topography data
library(ggplot2)     # plotting functions
library(vmstools)
library(RColorBrewer)
library(viridis)
## required libraries 


#-----------  COMBINING ALL COUNTRIES & CHANGING RESOLUTIONS  ------------

folder.path <- "C:/Work/Herring_VMS/"
## set location where your mapping_script output files are 

years <- c(2015, 2016, 2017, 2018, 2019, 2020) 

#years <- 2018
## we are more interested in recent years, so lets see if we can use 2014 onwards 

country <- c("UK", "NLD", "NO", "SW", "DEU", "FRA")

mesh.sizes <- c("Over 32mm", "Under 32mm")

## and we are going to have two mesh size categories, above and below 100mm 

data(europa) 
europa_sf <- st_as_sf(europa)
## and coastline file as sf object

EEZ <- st_read("C:/Work/GIS/World_EEZ_v11_20191118/eez_boundaries_v11.shp") %>% 
  st_set_crs(st_crs(europa_sf))
## add EEZ shapefile (download @: http://www.marineregions.org/downloads.php)

ICES <- st_read("C:/Work/GIS/Shapefiles/ICES Areas/ICES_Areas_20160601_cut_dense_3857.shp") %>% 
  st_set_crs(st_crs(europa_sf))

load("C:/Work/Herring_VMS/transfer.sf.rData")
transfer.sf <- transfer.sf %>%
  st_set_crs(st_crs(europa_sf))



k <- 2          # k <- 2 would give 15 nm grid cell size
## if wanting to aggregate data to lower resolution grid cells
## https://www.nhc.noaa.gov/gccalc.shtml (Latitude/Longitude Distance Calculator)

m <- 0          # m <- 3 would give 2.5 nm grid cell size
## can disaggergate to a 2.5 nm cell size if wanting to then aggregate in 2.5 nm units (i.e. 10 nm, 20 nm)


#pdf(file = paste(folder.path, "Pelagic_Maps_HER_Small.pdf", sep = ""), width = 4) 

for(i in 1:length(years)){ 
  
  ### Q1
  
  raster.list <- NULL
  
  for(j in 1:3){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  x_coord <- c(2.5,  2.5,  12, 12, 2.5)
  y_coord <- c(55, 60, 60, 55, 55)
  xym <- cbind(x_coord, y_coord)
  p = Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  sps_sf <- st_as_sf(sps)
  
  zoom.Max <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=max)
  zoom.Sum <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=sum)
  
   w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q1 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q1_", years[i], ".jpg", sep=""), w, width = 8, units ="in")

  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q1 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q1_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")

  ## Q2
  
  raster.list <- NULL
  
  for(j in 4:6){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q2 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q2_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  

  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q2 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q2_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
 
  
   ## Q3
  
  raster.list <- NULL
  
  for(j in 7:9){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q3 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q3_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
    
 w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q3 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
 
 ggsave(paste("maps/HER_-32mm_Q3_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
 
  
  
  ###  Q4
  
  raster.list <- NULL
  
  for(j in 10:12){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  
 w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q4 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q4_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q4 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_-32mm_Q4_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
  
}

dev.off()

##############################################################################

####  32mm+ Gears

#pdf(file = paste(folder.path, "Pelagic_Maps_HER_Large.pdf", sep = "")) 

for(i in 1:length(years)){ 
  
  ### Q1
  
  raster.list <- NULL
  
  for(j in 1:3){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_over32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA) 
  
 w <- ggplot() +
   geom_sf(data=ICES, fill="transparent",col="black") +
   geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
   #geom_sf(data=sps_sf,col="red",fill="transparent") +
   geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
   geom_sf(data=europa_sf) +
   #geom_sf(data=EEZ,col="white") +
   coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
   scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
   ggtitle(paste("Herring, Q1 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
           subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
   labs(x = "Longitude", y= "Latitude",fill="(t)") +
   theme_bw() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
 
  ggsave(paste("maps/HER_+32mm_Q1_", years[i], ".jpg", sep=""), w, width = 8, units ="in") 
  
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    #geom_sf(data=EEZ,col="white") +
    geom_sf(data=europa_sf) +
    coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
    scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
    ggtitle(paste("Herring, Q1 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
            subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    ggsave(paste("maps/HER_+32mm_Q1_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
  
  
  ## Q2
  
  raster.list <- NULL
  
  for(j in 4:6){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_over32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  x_coord <- c(2.5,  2.5,  12, 12, 2.5)
  y_coord <- c(55, 60, 60, 55, 55)
  xym <- cbind(x_coord, y_coord)
  p = Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  sps_sf <- st_as_sf(sps)
  
  zoom.Max <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=max)
  zoom.Sum <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=sum)
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q2 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_+32mm_Q2_", years[i], "_.jpg", sep=""), w, width = 8, units ="in")
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q2 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_+32mm_Q2_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
  
  
  ## Q3
  
  raster.list <- NULL
  
  for(j in 7:9){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_over32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  
  
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  x_coord <- c(2.5,  2.5,  12, 12, 2.5)
  y_coord <- c(55, 60, 60, 55, 55)
  xym <- cbind(x_coord, y_coord)
  p = Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  sps_sf <- st_as_sf(sps)
  
  zoom.Max <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=max)
  zoom.Sum <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=sum)
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q3 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_+32mm_Q3_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q3 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_+32mm_Q3_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")
  
  ###  Q4
  
  raster.list <- NULL
  
  for(j in 10:12){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Herring_.*._", month.name[j], "_", years[i], "_over32mm.gri$",sep=""), full.names=TRUE))
  }
  
  
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  x_coord <- c(2.5,  2.5,  12, 12, 2.5)
  y_coord <- c(55, 60, 60, 55, 55)
  xym <- cbind(x_coord, y_coord)
  p = Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  sps_sf <- st_as_sf(sps)
  
  zoom.Max <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=max)
  zoom.Sum <- extract(cod.sum,sps_sf,na.rm=TRUE, fun=sum)
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
          #geom_sf(data=sps_sf,col="red",fill="transparent") +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          geom_sf(data=europa_sf) +
          #geom_sf(data=EEZ,col="white") +
          coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
          scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q4 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
  ggsave(paste("maps/HER_+32mm_Q4_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  w <- ggplot() +
          geom_sf(data=ICES, fill="transparent",col="black") +
          geom_sf(data=transfer.sf,fill="white",linetype = "11", size = 1) +
          geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
          #geom_sf(data=EEZ,col="white") +
          geom_sf(data=europa_sf) +
          coord_sf(xlim = c(2.5, 12), ylim = c(55, 60),expand=FALSE) +
          scale_fill_viridis(option="plasma",limits = c(0.1, zoom.Max), direction=-1,na.value="transparent") +
          ggtitle(paste("Herring, Q4 ", years[i], " " ,mesh.sizes[1]," gears", sep =""),
                  subtitle = paste("Total = ", round(zoom.Sum, 0), "t", sep=" ")) +
          labs(x = "Longitude", y= "Latitude",fill="(t)") +
          theme_bw() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
   ggsave(paste("maps/HER_+32mm_Q4_", years[i], "_(transfer_area).jpg", sep=""), w, width = 8, units ="in")

  
}

dev.off()





##############################################################################

####  NORWAY POUT



#pdf(file = paste(folder.path, "Pelagic_Maps_NOP_Small.pdf", sep = "")) 

for(i in 1:length(years)){ 
  
  ### Q1
  
  raster.list <- NULL
  
  for(j in 1:3){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Norway_pout_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = "")) 
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Norway Pout, Q1 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/NOP_-32mm_Q1_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  
  ## Q2
  
  raster.list <- NULL
  
  for(j in 4:6){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Norway_pout.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = ""))
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Norway Pout, Q2 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
 ggsave(paste("maps/NOP_-32mm_Q2_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
 
  
  ## Q3
  
  raster.list <- NULL
  
  for(j in 7:9){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Norway_pout_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = ""))
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Norway Pout, Q3 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/NOP_-32mm_Q3_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  
  ###  Q4
  
  raster.list <- NULL
  
  for(j in 10:12){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Norway_pout.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = "")) 
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Norway Pout, Q4 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
 ggsave(paste("maps/NOP_-32mm_Q4_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
 
}

dev.off()

##############################################################################



##############################################################################

####  SPRAT



# pdf(file = paste(folder.path, "Pelagic_Maps_SPR_Small.pdf", sep = "")) 

for(i in 1:length(years)){ 
  
  ### Q1
  
  raster.list <- NULL
  
  for(j in 1:3){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Sprat_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = "")) 
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Sprat, Q1 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/SPR_-32mm_Q1_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  
  ## Q2
  
  raster.list <- NULL
  
  for(j in 4:6){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Sprat_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = ""))
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Sprat, Q2 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/SPR_-32mm_Q2_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
  
  ## Q3
  
  raster.list <- NULL
  
  for(j in 7:9){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Sprat_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = ""))
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Sprat, Q3 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/SPR_-32mm_Q3_", years[i], ".jpg", sep=""), w, width = 8, units ="in")

  
  ###  Q4
  
  raster.list <- NULL
  
  for(j in 10:12){  
    raster.list <-c(raster.list, list.files(path=paste(folder.path, "/results", sep=""), 
                                            pattern =paste("Sprat_.*._", month.name[j], "_", years[i], "_under32mm.gri$",sep=""), full.names=TRUE))
  }
  
  raster.list <- c(raster.list, paste(folder.path, "results/null_raster.gri", sep = "")) 
  raster.stack <- stack(raster.list)
  ## read in and create a RasterStack }
  
  raster.stack[is.na(raster.stack)] <- 0
  ## replace NA values with 0 to allow calculation
  
  cod.sum <- calc(raster.stack, sum)
  ## sum raster layers of raster stack and return a 'total' RasterLayer
  
  if(m > 0){
    cod.sum <- disaggregate(cod.sum,fact=m,
                            fun=sum)
  }
  
  if(k > 0){
    cod.sum <- aggregate(cod.sum,fact=k,
                         fun=sum)
  }
  
  cod.df <- as.data.frame(cod.sum, xy=TRUE) # Convert to data.frame, keeping the coordinates
  ## plotting in ggplot requires raster as a data frame
  cod.df$layer <- replace(cod.df$layer,cod.df$layer == 0, NA)
  
  w <- ggplot() +
    geom_sf(data=ICES, fill="transparent",col="black") +
    geom_sf(data=transfer.sf,fill="transparent",linetype = "11", size = 1) +
    #geom_sf(data=sps_sf,col="red",fill="transparent") +
    geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
    geom_sf(data=europa_sf) +
    #geom_sf(data=EEZ,col="white") +
    coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
    scale_fill_viridis(option="plasma",direction=-1,na.value="transparent") +
    ggtitle(paste("Sprat, Q4 ", years[i], " " ,mesh.sizes[2]," gears", sep =""),
            subtitle = paste("Total = ", round(sum(raster.stack@data@values), 0), "t", sep=" ")) +
    labs(x = "Longitude", y= "Latitude",fill="(t)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(paste("maps/SPR_-32mm_Q4_", years[i], ".jpg", sep=""), w, width = 8, units ="in")
  
}

dev.off()

##############################################################################
