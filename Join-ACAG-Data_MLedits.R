# Join ACAG PM2.5 data to the PEGS cohort data

PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)





# Some fake monitor coordinate locations
#monitor.locs <- data.frame("Long" = c(-79,-100),"Lat" = c(35.9,36))

monitor.locs <- data.frame("Long" = PEGS2$geo_longitude,"Lat" = PEGS2$geo_latitude)
#ok, so one group is concentration and the other is percent...this will be helpful later on. 
test1 <- raster:::raster("/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/GWRwSPEC_BC_NA_200001_200012.nc")

test2 <- raster:::raster("/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/GWRwSPEC.HEI_BCp_NA_201701_201712-wrtSPECtotal.nc")


##### PM2.5  

PM25.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/PM25/"

# Get Raster data filenames/paths

PM25.filenames_perc <- list.files(path=PM25.dir,
                                 pattern="PM25p", full.names=T)

PM25.filenames_conc <- list.files(path=SO4.dir,
                                 pattern="PM25_", full.names=T)

# Get raster bricks for each year using lapply
PM25.bricks_p <- lapply(PM25.filenames_perc,raster::brick,nl = 1)

PM25.bricks_c <- lapply(PM25.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.SO4p <- lapply(PM25.bricks_p,raster::extract,monitor.locs)
monitors.SO4 <- lapply(PM25.bricks_c,raster::extract,monitor.locs)


##### Sulfate  

SO4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/SO4/"


# Get Raster data filenames/paths

SO4.filenames_perc <- list.files(path=SO4.dir,
                                pattern="SO4p", full.names=T)

SO4.filenames_conc <- list.files(path=SO4.dir,
                                pattern="SO4_", full.names=T)

# Get raster bricks for each year using lapply
SO4.bricks_p <- lapply(SO4.filenames_perc,raster::brick,nl = 1)

SO4.bricks_c <- lapply(SO4.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.SO4p <- lapply(SO4.bricks_p,raster::extract,monitor.locs)
monitors.SO4 <- lapply(SO4.bricks_c,raster::extract,monitor.locs)


##### Organic Matter  


OM.dir <-  "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/OM/"

# Get Raster data filenames/paths

OM.filenames_perc <- list.files(path=OM.dir,
                                 pattern="OMp", full.names=T)

OM.filenames_conc <- list.files(path=OM.dir,
                                 pattern="OM_", full.names=T)

# Get raster bricks for each year using lapply
OM.bricks_p <- lapply(OM.filenames_perc,raster::brick,nl = 1)

OM.bricks_c <- lapply(OM.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.OMp <- lapply(OM.bricks_p,raster::extract,monitor.locs)
monitors.OMc <- lapply(OM.bricks_c,raster::extract,monitor.locs)




##### Nitrate  

NIT.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NIT/"

# Get Raster data filenames/paths

NIT.filenames_perc <- list.files(path=NIT.dir,
                                pattern="NITp", full.names=T)

NIT.filenames_conc <- list.files(path=NIT.dir,
                                pattern="NIT_", full.names=T)

# Get raster bricks for each year using lapply
NIT.bricks_p <- lapply(NIT.filenames_perc,raster::brick,nl = 1)

NIT.bricks_c <- lapply(NIT.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.NITp <- lapply(NIT.bricks_p,raster::extract,monitor.locs)
monitors.NITc <- lapply(NIT.bricks_c,raster::extract,monitor.locs)


##### Black Carbon  

BC.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/"
# Get Raster data filenames/paths
#BC.filenames <- list.files(path = BC.dir, full.names = TRUE)

BC.filenames_perc <- list.files(path=BC.dir,
                                pattern="BCp", full.names=T)
BC.filenames_conc <- list.files(path=BC.dir,
                                pattern="BC_", full.names=T)


# Get raster bricks for each year using lapply
BC.bricks_p <- lapply(BC.filenames_perc,raster::brick,nl = 1)
BC.bricks_c <- lapply(BC.filenames_conc,raster::brick,nl = 1)


# Extract the data at monitor locations for each year
monitors.BCp <- lapply(BC.bricks_p,raster::extract,monitor.locs)
monitors.BCc <- lapply(BC.bricks_c,raster::extract,monitor.locs)



##### Ammonium ion  

NH4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NH4/"

# Get Raster data filenames/paths


NH4.filenames_perc <- list.files(path=NH4.dir,
                                pattern="NH4p", full.names=T)
NH4.filenames_conc <- list.files(path=NH4.dir,
                                pattern="NH4_", full.names=T)


# Get raster bricks for each year using lapply
NH4.bricks_p <- lapply(NH4.filenames_perc,raster::brick,nl = 1)
NH4.bricks_c <- lapply(NH4.filenames_conc,raster::brick,nl = 1)


# Extract the data at monitor locations for each year
monitors.NH4p <- lapply(NH4.bricks_p,raster::extract,monitor.locs)
monitors.NH4c <- lapply(NH4.bricks_c,raster::extract,monitor.locs)
