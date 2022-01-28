# Join ACAG PM2.5 data to the PEGS cohort data


data11  <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/Surveys/Health_and_Exposure/healthexposure_02jan20_fmtd_v1.csv")

data22 <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/Surveys/Exposome/exposomeb_02jan20_fmtd_v1.csv")

#this is the health and exposure data

geocodes <- read.csv("/Volumes/SHAG/PEGS_AirPollutionExposure/PEGS_DATA/PEGS_addresses4ML.csv") #this cleaned address data

demo <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/Map/bcbb_map_02jan20_fmtd_v1.csv") #this all of demographics


library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(sp)
library(RColorBrewer)
library(tidyverse)
demo2 <- subset(demo, demo$epr_number %in% data11$epr_number)
demo3 <- subset(demo2, select=c(epr_number,age_derived, gender, race, ethnicity))
demo3$age_derived <- as.numeric(as.factor(ifelse(demo3$age_derived == "MISSING",NA,demo3$age_derived)))
table(demo3$age_derived)



#diabetestype <- subset(diabetes, select=c(epr_number, ds_diabetes_type_CHILDQ))
#table(as.factor(diabetestype$ds_diabetes_type_CHILDQ))
hyper <- subset(data11, select= c(epr_number,he_b007_hypertension_PARQ, X_he_gender_, he_a001b_height_in, he_a002_weight, he_bmi_derived,
                                    he_bmi_cat_derived,  he_s180b_smoke_yrs_CHILDQ,he_t203_income,he_s179_100_cigarettes_PARQ))
hyper2 <- merge(hyper, demo3, by.x="epr_number", by.y="epr_number", all.x=T)





epr_geocodes <- geocodes$epr_number
length(unique(epr_geocodes)) #9765

table(geocodes$geo_study_event)

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
geocodes$epr_number_TYPE <- ifelse(geocodes$geo_study_event == "Exposome Part A - Current Address", paste0(geocodes$epr_number, "_2"),paste0(geocodes$epr_number, "_1"))


geocode_lim <- subset(geocodes, geocodes$geo_study_event == "Health and Exposure Survey")

final_hyper <- merge(geocode_lim, hyper2, by.x="epr_number", by.y="epr_number", all.y=T)



nc_map <-rgdal::readOGR(dsn="~/Documents/tl_2016_37_place")

nc_map2 <- spTransform(nc_map, CRS("+proj=longlat +datum=NAD83 +no_defs"))
#spdf <- SpatialPointsDataFrame(coords = monitor.locs, data = monitor.locs,
#                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

extent(nc_map2)

final_hyper2 <- subset(final_hyper, is.na(final_hyper$geo_longitude) == F)
# longitude and latitude (x,y) coordinates

acag3 <- st_as_sf(final_hyper2, coords=c("geo_longitude", "geo_latitude"))



acag3 <- as(acag3, 'Spatial')

acag3 <- crop(acag3, extent(nc_map2)) # transform CRS
library(leaflet)
library(maps)
table(acag3$he_b007_hypertension_PARQ)





acag3$he_b007_hypertension_PARQ <- ifelse(acag3$he_b007_hypertension_PARQ == "MISSING",NA,acag3$he_b007_hypertension_PARQ)





# Create a palette that maps factor levels to colors
pal <- colorFactor(c("lightblue", "blue"), domain = c("No", "Yes"))

leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    radius=3,
    color = ~pal(he_b007_hypertension_PARQ),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~he_b007_hypertension_PARQ,
                 title = "Hypertension Diagnosis",
                 labFormat = labelFormat(prefix = ),
                 opacity = 1)

table(acag3$he_t203_income)
acag3$he_t203_income <- ifelse(acag3$he_t203_income == "MISSING",NA,acag3$he_t203_income)

levels(as.factor(acag3$he_t203_income))
acag3$he_t203_income <- ifelse(acag3$he_t203_income == "$70,000 to 79,999" | acag3$he_t203_income == "$80,000 or more" , "$70,000 or more", acag3$he_t203_income)
acag3$he_t203_income <- ifelse(acag3$he_t203_income == "Less than $20,000", "$20,000 or less", acag3$he_t203_income)


acag3$income <- ordered(acag3$he_t203_income, levels = c("$20,000 or less", "$20,000 to 29,999", "$30,000 to 39,999" ,"$40,000 to 49,999" ,"$50,000 to 59,999",
                                                         "$60,000 to 69,999", "$70,000 or more"))

table(acag3$income)
pal <- colorFactor("magma", domain = c("$20,000 or less", "$20,000 to 29,999", "$30,000 to 39,999" ,"$40,000 to 49,999" ,"$50,000 to 59,999",  "$60,000 to 69,999", "$70,000 or more"))

leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    radius=6,
    color = ~pal(income),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~income,
                  title = "Reported Income",
                  labFormat = labelFormat(prefix = ),
                  opacity = 1)






pal <-  colorBin("magma", domain = 0:1)
leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(he_b007_hypertension_PARQ),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~he_b007_hypertension_PARQ,
          title = "Hypertension Diagnosis",
          labFormat = labelFormat(prefix = "$"),
          opacity = 1
)
pal <-  colorBin("magma", domain = 0:100)

leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(X2016_p_SO4.x),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~X2016_p_SO4.x,
                  title = "Est. SO4 Share 2016",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1
  )


pal <-  colorNumeric("magma", domain = 0:10)
no24 <- subset(no23, no23$X2008 <=10)
leaflet(no24) %>% addTiles() %>%
  addCircleMarkers(radius=3,
    color = ~pal(X2008),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~X2008,
                  title = "Est. NO2 Conc. \n 2008 (\u00b5g/m\u00b3)",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 1)

range(as.numeric(no2$X2008), na.rm=T)







