###libraries ###

library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(tidyr)
library(dplyr)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(sp)
library(rgeos)
library(nngeo)

###import PEGS addresses ###
PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
#CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
spdf2 <- spTransform(spdf,  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))



files.dir <- "~/Desktop/TRI/tri_val" 

filenames_TRI <- list.files(path=files.dir, full.names=T) 

TOLUENE <- NULL
BENZENE <- NULL
XYLENE <- NULL
EB <- NULL
for (i in 1:length(filenames_TRI)){
#for (i in 1:3){
  name1 <- (filenames_TRI[i])
  print(name1)
  TRI_a <- read.csv(name1)
  
  TOLUENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "TOLUENE" | TRI_a$X36..CAS...COMPOUND.ID == "108883")
  
  BENZENE_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "BENZENE"| TRI_a$X36..CAS...COMPOUND.ID == "71432")
  
  XYLENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "XYLENE (MIXED ISOMERS)"| TRI_a$X36..CAS...COMPOUND.ID == "108383" | TRI_a$X36..CAS...COMPOUND.ID == "1330207")
  
  EB_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "ETHYLBENZENE" | TRI_a$X36..CAS...COMPOUND.ID == "100414")
  
  TOLUENE <- as.data.frame(rbind(TOLUENE, TOLUENE_a))
  BENZENE <- as.data.frame(rbind(BENZENE, BENZENE_a))
  XYLENE <- as.data.frame(rbind(XYLENE, XYLENE_a))
  EB <- as.data.frame(rbind(EB, EB_a))
  
}




TOLUENE2 <- subset(TOLUENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
BENZENE2 <- subset(BENZENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
XYLENE2 <- subset(XYLENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
EB2 <- subset(EB, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))

states <- c("AL","AZ","AR","CA", "CO", "CT", "DE", "FL","GA","ID","IL","IN","IA", "KS","KY","LA","ME","MD","MA","MI",
          "MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX", "UT","VT", "VA","WA","WV","WI","WY","DC")
TOLUENE2 <- subset(TOLUENE2, TOLUENE2$X8..ST %in% states)
BENZENE2 <- subset(BENZENE2, BENZENE2$X8..ST %in% states)
XYLENE2 <- subset(XYLENE2, XYLENE2$X8..ST %in% states)
EB2 <- subset(EB2, EB2$X8..ST %in% states)

TOLUENE2$location <- paste0(TOLUENE2$X12..LATITUDE, TOLUENE2$X13..LONGITUDE)
length(unique(TOLUENE2$location))
BENZENE2$location <- paste0(BENZENE2$X12..LATITUDE, BENZENE2$X13..LONGITUDE)
length(unique(BENZENE2$location))
XYLENE2$location <- paste0(XYLENE2$X12..LATITUDE, XYLENE2$X13..LONGITUDE)
length(unique(XYLENE2$location))
EB2$location <- paste0(EB2$X12..LATITUDE, EB2$X13..LONGITUDE)
length(unique(EB2$location))



#write.csv(TOLUENE2, file="~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv", row.names=F) #all addresses
#write.csv(BENZENE2, file="~/Desktop/TRI/tri_tot/BENZENE_allyears.csv", row.names=F) #all addresses
#write.csv(XYLENE2, file="~/Desktop/TRI/tri_tot/XYLENE_allyears.csv", row.names=F) #all addresses
#write.csv(EB2, file="~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv", row.names=F) #all addresses


#Melissa will do research on finding mean values across single point over multiple years.

# TOLUENE2 <- read.csv("~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv") #all addresses
#BENZENE2 <- read.csv("~/Desktop/TRI/tri_tot/BENZENE_allyears.csv") #all addresses
#XYLENE2 <- read.csv("~/Desktop/TRI/tri_tot/XYLENE_allyears.csv") #all addresses
#EB2 <- read.csv("~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv") #all addresses

length(unique(TOLUENE2$location)) #3172
length(unique(BENZENE2$location)) # 1755
length(unique(XYLENE2$location)) #5568
length(unique(EB2$location)) #3172



#Turn these into spatial points dataframes


names(TOLUENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(BENZENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(XYLENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(EB2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")

TOLUENE25 <- unique(subset(TOLUENE2, select=c("latitude","longitude", "facility", "location")))
BENZENE25 <- unique(subset(BENZENE2, select=c("latitude","longitude", "facility", "location")))
XYLENE25 <- unique(subset(XYLENE2, select=c("latitude","longitude", "facility", "location")))
EB25 <- unique(subset(EB2, select=c("latitude","longitude", "facility", "location")))


TOLUENE3 <- sf:::as_Spatial(st_as_sf(TOLUENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
BENZENE3 <- sf:::as_Spatial(st_as_sf(BENZENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
XYLENE3 <- sf:::as_Spatial(st_as_sf(XYLENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
EB3 <- sf:::as_Spatial(st_as_sf(EB25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))

TOLUENE4 <- spTransform(TOLUENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
BENZENE4 <- spTransform(BENZENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
XYLENE4 <- spTransform(XYLENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
EB4 <- spTransform(EB3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 


#sparse matrices? 
#determine the size in bytes - convert to mb or gb and run a loop to create matrices of different sizes

matrixtest_t <- pointDistance(TOLUENE4, spdf2, lonlat=F, allpairs=T)
colnames(matrixtest_t) <- spdf2$epr_number_TYPE
rownames(matrixtest_t) <- TOLUENE4$location

matrixtest_b <- pointDistance(BENZENE4, spdf2, lonlat=F, allpairs=T)
colnames(matrixtest_b) <- spdf2$epr_number_TYPE
rownames(matrixtest_b) <- BENZENE4$location

matrixtest_x <- pointDistance(XYLENE4, spdf2, lonlat=F, allpairs=T)
colnames(matrixtest_x) <- spdf2$epr_number_TYPE
rownames(matrixtest_x) <- XYLENE4$location

matrixtest_eb <- pointDistance(EB4, spdf2, lonlat=F, allpairs=T)
colnames(matrixtest_eb) <- spdf2$epr_number_TYPE
rownames(matrixtest_eb) <- EB4$location


psEXP <- function(d,C0,decay) { 
  # d : distance matrix (m x n) m = # of monitoring sites, n = # of sources
  #  C0 : initial source concentrations
  # decay: vector of buffer values (presumably in the same units as d)
  X.ps <- matrix(NA,nrow = nrow(d),ncol = length(decay))
  
  decay.func <- function(x) sum(C0 * exp(-3 * x / decay[i]))
  
  for (i in 1:length(decay)){
    X.ps[,i] <- d %>% apply(1,decay.func)
  }
  
  return(X.ps)
  
}
