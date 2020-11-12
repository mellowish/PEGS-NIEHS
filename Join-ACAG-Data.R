# Join ACAG PM2.5 data to the PEGS cohort data
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)

# Some fake monitor coordinate locations
monitor.locs <- data.frame("Long" = c(-79,-100),"Lat" = c(35.9,36))

##### PM2.5  

PM25.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/PM25/"
# Get Raster data filenames/paths
PM25.filenames <- list.files(path = PM25.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
PM25.bricks <- lapply(PM25.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.PM25 <- lapply(PM25.bricks,raster::extract,monitor.locs)

# Do a check of the air pollution data and our monitoring data locations
# US.PM25 <- aggregate(PM25.bricks[[1]]$layer, fact=10)
# plot(US.PM25)
# points(monitor.locs)
# All good!


##### Sulfate  

SO4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/SO4/"
# Get Raster data filenames/paths
SO4.filenames <- list.files(path = SO4.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
SO4.bricks <- lapply(SO4.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.SO4 <- lapply(SO4.bricks,raster::extract,monitor.locs)


##### Organic Matter  

OM.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/OM/"
# Get Raster data filenames/paths
OM.filenames <- list.files(path = OM.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
OM.bricks <- lapply(OM.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.OM <- lapply(OM.bricks,raster::extract,monitor.locs)


##### Nitrate  

NIT.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NIT/"
# Get Raster data filenames/paths
NIT.filenames <- list.files(path = NIT.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
NIT.bricks <- lapply(NIT.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.NIT <- lapply(NIT.bricks,raster::extract,monitor.locs)

##### Black Carbon  

BC.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/"
# Get Raster data filenames/paths
BC.filenames <- list.files(path = BC.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
BC.bricks <- lapply(BC.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.BC <- lapply(BC.bricks,raster::extract,monitor.locs)

##### Ammonium ion  

NH4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NH4/"
# Get Raster data filenames/paths
NH4.filenames <- list.files(path = NH4.dir, full.names = TRUE)

# Get raster bricks for each year using lapply
NH4.bricks <- lapply(NH4.filenames,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.NH4 <- lapply(NH4.bricks,raster::extract,monitor.locs)