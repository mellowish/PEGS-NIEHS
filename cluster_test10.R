###  for Submission to Cluster ###
### Making 10km buffers around addresses

#import addresses
PEGS3 <- read.csv("/ddn/gs1/group/shag/loweme/pegs_majorroads/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude)) #remove any missing

#libraries
library(sp)
library(rgeos)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")] #extract longitude and latitude
#transform to spatial data frame
spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
#CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#project to alberts equal area conic so that we can calculate euclidean distances
spdf2 <- spTransform(spdf,  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


#import roads spatial data
roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")
roads@proj4string
#project to alberts equal area conic so that we can calculate euclidean distances

roads <- spTransform(roads, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 



  l <- split(spdf2, spdf2$epr_number_TYPE)  #split spdf line by line so that each address functions alone
  #create 10km buffer around each address which is a unique component of the list l
  lbuff_10km <- lapply(l, gBuffer, width = 10000)

  e <- NA #make an empty item
  rd_10kmfun <- function(i){
    tryCatch(raster::crop(roads, extent(lbuff_10km[[i]])), error = function(e) e)
    #trycatch is used to manage the errors where there are no roads within 10km, "simpleError" is what is left
    #crop the roads around each address
    
     }
  

  rd_10km <- lapply(1:length(spdf2), rd_10kmfun)
  #apply the function across spdf2
  
  totrd_10kmfun <- function(i){
    
    ifelse(class(rd_10km[[i]])[1] == "simpleError", NA, gLength(rd_10km[[i]], byid=F))
    #find total road length in buffer if there are roads within the buffer
    #otherwise, NA. "simpleError" is produced by the trycatch when it fails
  }
  
  totrd_10km <- lapply(1:length(spdf2), totrd_10kmfun) # lapply statement with that function
  

  save(rd_10km, file = "/ddn/gs1/group/shag/loweme/pegs_majorroads/rd_10kmtry.RData")
  save(totrd_10km,file = "/ddn/gs1/group/shag/loweme/pegs_majorroads/totrd_10kmtry.RData")
  
