#### New attempt at managing the major roads question ###

### Started February 8 2021 #####

### Libraries ####

library(sp)
library(rgeos)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(sf)

#### IMPORT THE DATA ####
### TWO OPTIONS: one for the cluster and one for testing ###

#PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")

#PEGS3 <- read.csv("/ddn/gs1/group/shag/loweme/pegs_majorroads/PEGS_CensusTractAddresses.csv")

#roads <-rgdal::readOGR(dsn="/Volumes/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")

#roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")

PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))
#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))


### Roads is coming in as a spatial lines dataframe.

### PEGS should be transformed into a spatial points dataframe.

xy <- PEGS2[,c("geo_longitude","geo_latitude")] #longitude, then latitude

spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

## In order to create buffers, we need to transform the crs to something flat. 
## Arguably, this should be dependent on the location of the item in PEGS. 
