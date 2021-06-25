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
## required libraries 


#-----------  COMBINING ALL COUNTRIES & CHANGING RESOLUTIONS  ------------

folder.path <- "F:/WGTM/ToR b-c/data/"
## set location where your mapping_script output files are 

years <- c(2014, 2015, 2016, 2017, 2018) 
## we are more interested in recent years, so lets see if we can use 2014 onwards 

mesh.sizes <- c("below_100mm", "100mm_plus") 
## and we are going to have two mesh size categories, above and below 100mm 

data(europa) 
europa_sf <- st_as_sf(europa)
## and coastline file as sf object

EEZ <- st_read("F:/WGTM/ToR b-c/World_EEZ_v11_20191118/eez_boundaries_v11.shp") %>% 
  st_set_crs(st_crs(europa_sf))
## add EEZ shapefile (download @: http://www.marineregions.org/downloads.php)


k <- 2          # k <- 2 would give 15 nm grid cell size
## if wanting to aggregate data to lower resolution grid cells
## https://www.nhc.noaa.gov/gccalc.shtml (Latitude/Longitude Distance Calculator)

m <- 0          # m <- 3 would give 2.5 nm grid cell size
## can disaggergate to a 2.5 nm cell size if wanting to then aggregate in 2.5 nm units (i.e. 10 nm, 20 nm)



pdf(file = paste(folder.path, "Cod_Maps3.pdf", sep = "")) 
for(i in 1:length(years)){ 
  for(j in 1:length(mesh.sizes)){
    
    raster.list <- list.files(path=folder.path, 
                              pattern =paste(years[i],".*",mesh.sizes[j],".*.gri$",sep=""), full.names=TRUE)
    
    raster.stack <- stack(raster.list)
    ## read in and create a RasterStack 
    
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
    
    
    print(ggplot() +
            geom_raster(data = cod.df, aes(x = x, y = y, fill = layer)) +
            geom_sf(data=europa_sf) +
            geom_sf(data=EEZ,col="white") +
            coord_sf(xlim = c(-4, 12), ylim = c(48, 62),expand=FALSE) +
            scale_fill_viridis_c() +
            ggtitle(paste("all countries",years[i],mesh.sizes[j], sep=" ")) +
            labs(x = "Longitude", y= "Latitude") +
            theme_bw())
    ## create plot of combined TR1 and TR2 
    
  }
}

dev.off() 
## closes the pdf file 
