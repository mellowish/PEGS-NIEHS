### TRI for Submission to Cluster ###

PEGS3 <- read.csv("/ddn/gs1/group/shag/loweme/pegs_majorroads/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))

library(sp)
library(rgeos)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
#CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


spdf2 <- spTransform(spdf,  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))



roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")
roads@proj4string
roads <- spTransform(roads, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 



  l <- split(spdf2, spdf2$epr_number_TYPE) 
  lbuff_5km <- lapply(l, gBuffer, width = 5000)

  e <- NA
  rd_5kmfun <- function(i){
    tryCatch(raster::crop(roads, extent(lbuff_5km[[i]])), error = function(e) e)
  }
  

  rd_5km <- lapply(1:length(spdf2), rd_5kmfun)
  
  totrd_5kmfun <- function(i){
    
    ifelse(class(rd_5km[[i]])[1] == "simpleError", NA, gLength(rd_5km[[i]], byid=F))
    #find total road length in buffer if there are roads within the buffer
    #otherwise, NA
  }
  
  totrd_5km <- lapply(1:length(spdf2), totrd_5kmfun) # lapply statement with that function
  

  save(rd_5km, file = "/ddn/gs1/group/shag/loweme/pegs_majorroads/rd_5kmtry.RData")
  save(totrd_5km,file = "/ddn/gs1/group/shag/loweme/pegs_majorroads/totrd_5kmtry.RData")
  
  #profiling: helps determine where the slowest part of the code is. shows how much time spent in each function
 