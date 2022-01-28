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
library(mapview)
library(units)

#### IMPORT THE DATA ####
### TWO OPTIONS: one for the cluster and one for testing ###

###import PEGS addresses ###
PEGS3 <- read.csv("~/Desktop/PEGS_addresses4ML.csv")

PEGS3$geo_latitude <- as.numeric(PEGS3$geo_latitude)
PEGS3$geo_longitude <- as.numeric(PEGS3$geo_longitude)

PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")]


#roads <-rgdal::readOGR(dsn="/Volumes/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")

#roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")
roads2 <- read_sf('~/Desktop/roadtrl010g.shp_nt00920') #loads WAYYY Faster than rgdal


### Roads is coming in as a spatial lines dataframe.(sf object? how?)


projcrs <- "+proj=longlat +datum=NAD83 +no_defs"
df_sf <- st_as_sf(x = PEGS2,                         
               coords = c("geo_longitude", "geo_latitude"),
               crs = projcrs)

## In order to create buffers, we need to transform the crs to something flat. 
## Arguably, this should be dependent on the location of the item in PEGS. 


### What is the original CRS of roads?

proj4string(roads)
#[1] "+proj=longlat +datum=NAD83 +no_defs"
#Warning message:
#  In proj4string(roads) : CRS object has comment, which is lost in output

# there are a lot of changes happening to spatial R packages right now. the comment lost in output is probably part of that.


# we'll use albers equal area projection:
# "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#what if i try using sf only?

#convert to albers ea projection

sf2 <- df_sf %>% st_transform(crs = 5070)
crs(sf2)
crs(roads2)
roads3 <- roads2 %>% st_transform(crs = 5070)
crs(roads3)
density_1km <- NULL
density_5km <- NULL
density_10km <- NULL
counter <- NULL
nums <-NULL
library(tictoc)
tic()
for (i in 1:10623){
a <- i
b <- PEGS2$epr_number_TYPE[i]
cc <- PEGS2$epr_number[i]

#density_1km <- c(density_1km, cd)
sf2_buffer1 <-  st_buffer(sf2[i,], 1000)
sf2_buffer2 <-  st_buffer(sf2[i,], 5000)
sf2_buffer3 <-  st_buffer(sf2[i,], 10000)


#mapview(sf2_buffer2)

testcrop1 <- st_crop(roads3, sf2_buffer1)
testcrop2 <- st_crop(roads3, sf2_buffer2)
testcrop3 <- st_crop(roads3, sf2_buffer3)

#mapview(testcrop)

length_test1 <- sum(st_length(testcrop1))
length_test2 <- sum(st_length(testcrop2))
length_test3 <- sum(st_length(testcrop3))

counter <- c(counter, b)
nums <- c(nums, cc)


density_1km <- c(density_1km, length_test1)
density_5km <- c(density_5km, length_test2)
density_10km <- c(density_10km, length_test3)
}
toc()


data1 <- cbind(counter,nums, density_1km, density_5km, density_10km)

write.csv(data1, "~/Desktop/densityroad_7000_10623.csv") #all addresses

densityroad_7000_10623 <- read.csv("~/Desktop/densityroad_7000_10623.csv")
d1 <- densityroad_7000_10623 
densityroad_3500_7000 <- read.csv("~/Desktop/densityroad_3500_7000.csv")
d2 <- densityroad_3500_7000
epr_number1 <- NULL
for ( i in 1: nrow(d1)){
  d1_num <- d1$counter[i]
  epr_num <- unlist(str_split(d1_num, "_"))[2]
  epr_number1 <- c(epr_number1, epr_num)

}

d1$epr_number <- epr_number1

epr_number2 <- NULL
 for ( i in 1: nrow(d2)){
  d2_num <- d2$counter[i]
  epr_num <- unlist(str_split(d2_num, "_"))[2]
  epr_number2 <- c(epr_number2, epr_num)
  
 }
d2$epr_number <- epr_number2


d11 <- subset(d1, select=c("epr_number","density_1km","density_5km","density_10km"))
d22 <- subset(d2, select=c("epr_number","density_1km","density_5km","density_10km"))

d3 <- unique(merge(d11,d22, by.x=c("epr_number","density_1km","density_5km","density_10km"), by.y=c("epr_number","density_1km","density_5km","density_10km"), all=T))

write.csv(d3, "/Volumes/SHAG/PEGS_AirPollutionExposure/road_density.csv")

#write.csv(density_5km, "~/Desktop/density5km_10409_10623.csv") #all addresses
#write.csv(density_1km, "~/Desktop/density1km_10409_10623.csv") #all addresses

#d <- read.csv( "~/Desktop/names_10409_10623.csv") #all addressesc
#c <- read.csv( "~/Desktop/names_8499_10409.csv") #all addresses
#b <- read.csv("~/Desktop/names_3631_8499.csv") #all addresses
#a <- read.csv( "~/Desktop/names_1_3630_.csv") #all addresses

#names_tot <- rbind(a,b,c,d)


#d1km1 <- read.csv("~/Desktop/1_4879density1km.csv")

#d1km2 <- read.csv("~/Desktop/4880_7528density1km.csv")

#d1km3 <- read.csv("~/Desktop/7528_12340density1km.csv")

#dkm1tot <- rbind(d1km1,d1km2,d1km3)

#dkm1tot <- cbind(PEGS2$epr_number_TYPE, dkm1tot)
#write.csv(dkm1tot, "~/Desktop/roaddensity1km.csv") #all addresses

#d5km1 <- read.csv("~/Desktop/1_4879density5km.csv")

#d5km2 <- read.csv("~/Desktop/4880_7528density5km.csv")

#d5km3 <- read.csv("~/Desktop/7528_12340density5km.csv")

#dkm5tot <- rbind(d5km1,d5km2,d5km3)

#dkm5tot <- cbind(PEGS2$epr_number_TYPE, dkm5tot)

#write.csv(dkm5tot, "~/Desktop/roaddensity5km.csv") #all addresses


#d10km1 <- read.csv("~/Desktop/1_4879density10km.csv")

#d10km2 <- read.csv("~/Desktop/4880_7528density10km.csv")

#d10km3 <- read.csv("~/Desktop/7528_12340density10km.csv")

#dkm10tot <- rbind(d10km1,d10km2,d10km3)

#dkm10tot <- cbind(PEGS2$epr_number_TYPE, dkm10tot)

#write.csv(dkm10tot, "~/Desktop/roaddensity10km.csv") #all addresses


