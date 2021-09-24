library(tidyverse)
library(sf)
#> Linking to GEOS 3.6.2, GDAL 2.2.3, PROJ 4.9.3
library(nngeo)
library(mapview)
library(rgeos)
library(sp)


####################################################
# Calculating distance between soil sites and BRITPITS
####################################################

## Original data
soil <-read.csv("/Volumes/Personal/Metals/NSI_LOND_Urban_Soil_Data/NSI_LOND_URBAN.csv")
## Create sf objects from lat/lon points
soil_sf <- soil %>% st_as_sf(coords = c('Easting', 'Northing'), remove = T) %>%
  st_set_crs(27700) 

################
# Lead
lead <-read.csv("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_PB.csv")
lead_sf <- lead %>% st_as_sf(coords = c('EASTING', 'NORTHING'), remove = T) %>%
  st_set_crs(27700) 

# Use st_nearest_feature to cbind loc to stop by nearest points
## mutate to add column showing distance between geometries
lead_joined_sf <- soil_sf %>% 
  cbind(lead_sf[st_nearest_feature(soil_sf, lead_sf),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

# Save file as shp
st_write(lead_joined_sf, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_PB.shp")

################
# Arsenic
Arsenic <-read.csv("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_AS.csv")
Arsenic_sf <- Arsenic %>% st_as_sf(coords = c('EASTING', 'NORTHING'), remove = T) %>%
  st_set_crs(27700) 

# Use st_nearest_feature to cbind loc to stop by nearest points
## mutate to add column showing distance between geometries
Arsenic_joined_sf <- soil_sf %>% 
  cbind(Arsenic_sf[st_nearest_feature(soil_sf, Arsenic_sf),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

# Save file as shp
st_write(Arsenic_joined_sf, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_AS.shp")

################
# Cadmium
Cadmium <-read.csv("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_CD.csv")
Cadmium_sf <- Cadmium %>% st_as_sf(coords = c('EASTING', 'NORTHING'), remove = T) %>%
  st_set_crs(27700) 

# Use st_nearest_feature to cbind loc to stop by nearest points
## mutate to add column showing distance between geometries
Cadmium_joined_sf <- soil_sf %>% 
  cbind(Cadmium_sf[st_nearest_feature(soil_sf, Cadmium_sf),]) %>%
  mutate(dist = st_distance(geometry, geometry.1, by_element = T))

# Save file as shp
st_write(Cadmium_joined_sf, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_CD.shp")


################
# Plot the distances to nearest BRITPITS
# create a list on linestrings to plot on map (lines between monitoring station and nearest met station)
connected <- st_connect(soil_sf, Cadmium_sf)

# plot map of monitoring stations and nearest met station
mapview(connected) + 
  mapview(Cadmium_sf, color = 'red', col.region = 'pink') +
  mapview(soil_sf, color = 'black')

mapview(connected) + 
  mapview(soil_sf, color = 'red', col.region = 'pink') +
  mapview(Cadmium_sf, color = 'black')

####################################################

####################################################
# Counting number of BRITPITS within soil site buffer
#https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
####################################################
####################################################
# Lead 
####################################################

#Read in the Soil sites distance to Britpits PB shp.
soil_sf <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_PB.shp")

#Read in the Britpits PB shp.
lead <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_PB.shp")

#Buffer the soil samples 
soil_sf_50m = st_buffer(soil_sf, dist = 50)
#Count number of BRITPITS that intersect soil buffers
soil_sf_50m$count_pb_50m <- lengths(st_intersects(soil_sf_50m, lead))

#Buffer the soil samples 
soil_sf_100m = st_buffer(soil_sf, dist = 100)
#Count number of BRITPITS that intersect soil buffers
soil_sf_100m$count_pb_100m <- lengths(st_intersects(soil_sf_100m, lead))

#Buffer the soil samples 
soil_sf_1500m = st_buffer(soil_sf, dist = 1500)
#Count number of BRITPITS that intersect soil buffers
soil_sf_1500m$count_pb_1500m <- lengths(st_intersects(soil_sf_1500m, lead))

#Buffer the soil samples 
soil_sf_2000m = st_buffer(soil_sf, dist = 2000)
#Count number of BRITPITS that intersect soil buffers
soil_sf_2000m$count_pb_2000m <- lengths(st_intersects(soil_sf_2000m, lead))

# Merge different dataframes together 
#50m 
Lead_Final = soil_sf

soil_sf_50m_cut <- st_drop_geometry(soil_sf_50m)
soil_sf_50m_cut <- subset(soil_sf_50m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Arsenic, Cadmium, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Lead_Final <- merge(x = Lead_Final, y = soil_sf_50m_cut, by = "SAMPLE", all.x = TRUE)

#100m 
soil_sf_100m_cut <- st_drop_geometry(soil_sf_100m)
soil_sf_100m_cut <- subset(soil_sf_100m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Arsenic, Cadmium, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Lead_Final <- merge(x = Lead_Final, y = soil_sf_100m_cut, by = "SAMPLE", all.x = TRUE)

#1500m 
soil_sf_1500m_cut <- st_drop_geometry(soil_sf_1500m)
soil_sf_1500m_cut <- subset(soil_sf_1500m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Arsenic, Cadmium, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Lead_Final <- merge(x = Lead_Final, y = soil_sf_1500m_cut, by = "SAMPLE", all.x = TRUE)

#2000m 
soil_sf_2000m_cut <- st_drop_geometry(soil_sf_2000m )
soil_sf_2000m_cut <- subset(soil_sf_2000m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Arsenic, Cadmium, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Lead_Final <- merge(x = Lead_Final, y = soil_sf_2000m_cut, by = "SAMPLE", all.x = TRUE)
Lead_Final_output <- subset(Lead_Final, select=-c(Lead.y, Lead.x.1, Lead.y.1, Lead))
Lead_Final_output <- Lead_Final_output %>% rename(Lead = Lead.x)

st_write(Lead_Final_output, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Lead_Soil_BRITPITS_Final.shp")

####################################################
# Arsenic
####################################################

#Read in the Soil sites distance to Britpits AS shp.
soil_sf <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_AS.shp")

#Read in the Britpits AS shp.
arsenic <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_AS.shp")

#Buffer the soil samples 
soil_sf_50m = st_buffer(soil_sf, dist = 50)
#Count number of BRITPITS that intersect soil buffers
soil_sf_50m$count_as_50m <- lengths(st_intersects(soil_sf_50m, arsenic))

#Buffer the soil samples 
soil_sf_100m = st_buffer(soil_sf, dist = 100)
#Count number of BRITPITS that intersect soil buffers
soil_sf_100m$count_as_100m <- lengths(st_intersects(soil_sf_100m, arsenic))

#Buffer the soil samples 
soil_sf_1500m = st_buffer(soil_sf, dist = 1500)
#Count number of BRITPITS that intersect soil buffers
soil_sf_1500m$count_as_1500m <- lengths(st_intersects(soil_sf_1500m, arsenic))

#Buffer the soil samples 
soil_sf_2000m = st_buffer(soil_sf, dist = 2000)
#Count number of BRITPITS that intersect soil buffers
soil_sf_2000m$count_as_2000m <- lengths(st_intersects(soil_sf_2000m, arsenic))

# Merge different dataframes together 
#50m 
Arsenic_Final = soil_sf

soil_sf_50m_cut <- st_drop_geometry(soil_sf_50m)
soil_sf_50m_cut <- subset(soil_sf_50m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Cadmium, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Arsenic_Final <- merge(x = Arsenic_Final, y = soil_sf_50m_cut, by = "SAMPLE", all.x = TRUE)

#100m 
soil_sf_100m_cut <- st_drop_geometry(soil_sf_100m)
soil_sf_100m_cut <- subset(soil_sf_100m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Cadmium, 
                                                       layer, path, BGS_REFERE, PIT_NAME, 
                                                       ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                       TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                       LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                       AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                       EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                       dist))

Arsenic_Final <- merge(x = Arsenic_Final, y = soil_sf_100m_cut, by = "SAMPLE", all.x = TRUE)

#1500m 
soil_sf_1500m_cut <- st_drop_geometry(soil_sf_1500m)
soil_sf_1500m_cut <- subset(soil_sf_1500m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Cadmium, 
                                                         layer, path, BGS_REFERE, PIT_NAME, 
                                                         ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                         TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                         LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                         AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                         EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                         dist))

Arsenic_Final <- merge(x = Arsenic_Final, y = soil_sf_1500m_cut, by = "SAMPLE", all.x = TRUE)

#2000m 
soil_sf_2000m_cut <- st_drop_geometry(soil_sf_2000m )
soil_sf_2000m_cut <- subset(soil_sf_2000m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Cadmium, 
                                                         layer, path, BGS_REFERE, PIT_NAME, 
                                                         ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                         TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                         LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                         AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                         EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                         dist))

Arsenic_Final <- merge(x = Arsenic_Final, y = soil_sf_2000m_cut, by = "SAMPLE", all.x = TRUE)
Arsenic_Final_output <- subset(Arsenic_Final, select=-c(Arsenic.y, Arsenic.x.1, Arsenic.y.1, Arsenic))
Arsenic_Final_output <- Arsenic_Final_output %>% rename(Arsenic = Arsenic.x)

st_write(Arsenic_Final_output, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Arsenic_Soil_BRITPITS_Final.shp")

####################################################
# Cadmium
####################################################

#Read in the Soil sites distance to Britpits AS shp.
soil_sf <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Soil_distBritpits_CD.shp")

#Read in the Britpits AS shp.
cadmium <-st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Britpits_England_CD.shp")

#Buffer the soil samples 
soil_sf_50m = st_buffer(soil_sf, dist = 50)
#Count number of BRITPITS that intersect soil buffers
soil_sf_50m$count_as_50m <- lengths(st_intersects(soil_sf_50m, cadmium))

#Buffer the soil samples 
soil_sf_100m = st_buffer(soil_sf, dist = 100)
#Count number of BRITPITS that intersect soil buffers
soil_sf_100m$count_as_100m <- lengths(st_intersects(soil_sf_100m, cadmium))

#Buffer the soil samples 
soil_sf_1500m = st_buffer(soil_sf, dist = 1500)
#Count number of BRITPITS that intersect soil buffers
soil_sf_1500m$count_as_1500m <- lengths(st_intersects(soil_sf_1500m, cadmium))

#Buffer the soil samples 
soil_sf_2000m = st_buffer(soil_sf, dist = 2000)
#Count number of BRITPITS that intersect soil buffers
soil_sf_2000m$count_as_2000m <- lengths(st_intersects(soil_sf_2000m, cadmium))

# Merge different dataframes together 
#50m 
Cadmium_Final = soil_sf

soil_sf_50m_cut <- st_drop_geometry(soil_sf_50m)
soil_sf_50m_cut <- subset(soil_sf_50m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Arsenic, 
                                                     layer, path, BGS_REFERE, PIT_NAME, 
                                                     ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                     TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                     LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                     AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                     EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                     dist))

Cadmium_Final <- merge(x = Cadmium_Final, y = soil_sf_50m_cut, by = "SAMPLE", all.x = TRUE)

#100m 
soil_sf_100m_cut <- st_drop_geometry(soil_sf_100m)
soil_sf_100m_cut <- subset(soil_sf_100m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Arsenic, 
                                                       layer, path, BGS_REFERE, PIT_NAME, 
                                                       ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                       TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                       LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                       AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                       EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                       dist))

Cadmium_Final <- merge(x = Cadmium_Final, y = soil_sf_100m_cut, by = "SAMPLE", all.x = TRUE)

#1500m 
soil_sf_1500m_cut <- st_drop_geometry(soil_sf_1500m)
soil_sf_1500m_cut <- subset(soil_sf_1500m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Arsenic, 
                                                         layer, path, BGS_REFERE, PIT_NAME, 
                                                         ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                         TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                         LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                         AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                         EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                         dist))

Cadmium_Final <- merge(x = Cadmium_Final, y = soil_sf_1500m_cut, by = "SAMPLE", all.x = TRUE)

#2000m 
soil_sf_2000m_cut <- st_drop_geometry(soil_sf_2000m )
soil_sf_2000m_cut <- subset(soil_sf_2000m_cut, select=-c(SAMPLE_TYP,ATLAS_CODE, Lead, Arsenic, 
                                                         layer, path, BGS_REFERE, PIT_NAME, 
                                                         ALTERNATIV, PARENT_PIT, PIT_STATUS, 
                                                         TYPE_OF_MI, CMDTY_CODE, CMDTY_PROD,
                                                         LONGITUDE, LATITUDE, PIT_ADDRES, 
                                                         AGE, LITHOSTRAT, OPERATOR_N, DATE_UPDAT, 
                                                         EPR_CODE, PLANNING_R, MPA_CODE, SPONSOR_OR, 
                                                         dist))

Cadmium_Final <- merge(x = Cadmium_Final, y = soil_sf_2000m_cut, by = "SAMPLE", all.x = TRUE)
Cadmium_Final_output <- subset(Cadmium_Final, select=-c(Cadmium.y, Cadmium.x.1, Cadmium.y.1, Cadmium))
Cadmium_Final_output <- Cadmium_Final_output %>% rename(Cadmium = Cadmium.x)

st_write(Cadmium_Final_output, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Cadmium_Soil_BRITPITS_Final.shp")


####################################################
# Intersecting each soil site with underlying geology 
####################################################

####################################################
# Lead
####################################################

lead <- st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Lead_Soil_BRITPITS_Final.shp")

bedrock <- st_read("/Volumes/Personal/Metals/BGS_Geology_50_V8/Data/gb_50k_bedrock.shp")

lead_bedrock <- st_join(lead, bedrock)

lead_subset <- subset(lead_bedrock, select=-c(LEX_WEB, RANK, BED_EQ_D, MB_EQ_D, 
                                              FM_EQ_D, SUBGP_EQ_D, GP_EQ_D,
                                              SUPGP_EQ_D, SAMPLE_, path, ALTERNA, 
                                              PARENT_, SPONSOR, MAP_SRC, MAP_WEB,
                                              VERSION, RELEASED, LEX_RCS_I,
                                              NOM_SCALE, BGS_REF, NOM_BGS_YR,
                                              UUID, BGSTYPE, OPERATO, DATE_UP,
                                              EPR_COD, PLANNIN, MPA_COD))

st_write(lead_subset, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Lead_Soil_BRITPITS_Geology_Bedrock_Final.shp")

####################################################
# Arsenic
####################################################

arsenic <- st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Arsenic_Soil_BRITPITS_Final.shp")

bedrock <- st_read("/Volumes/Personal/Metals/BGS_Geology_50_V8/Data/gb_50k_bedrock.shp")

arsenic_bedrock <- st_join(arsenic, bedrock)

arsenic_subset <- subset(arsenic_bedrock, select=-c(LEX_WEB, RANK, BED_EQ_D, MB_EQ_D, 
                                                    FM_EQ_D, SUBGP_EQ_D, GP_EQ_D,
                                                    SUPGP_EQ_D, SAMPLE_, path, ALTERNA, 
                                                    PARENT_, SPONSOR, MAP_SRC, MAP_WEB,
                                                    VERSION, RELEASED, LEX_RCS_I,
                                                    NOM_SCALE, BGS_REF, NOM_BGS_YR,
                                                    UUID, BGSTYPE, OPERATO, DATE_UP,
                                                    EPR_COD, PLANNIN, MPA_COD))

st_write(arsenic_subset, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Arsenic_Soil_BRITPITS_Geology_Bedrock_Final.shp")

####################################################
# Cadmium
####################################################

cadmium <- st_read("/Volumes/Personal/Metals/BRITPITS/New_datasets/Cadmium_Soil_BRITPITS_Final.shp")

bedrock <- st_read("/Volumes/Personal/Metals/BGS_Geology_50_V8/Data/gb_50k_bedrock.shp")

cadmium_bedrock <- st_join(cadmium, bedrock)

cadmium_subset <- subset(cadmium_bedrock, select=-c(LEX_WEB, RANK, BED_EQ_D, MB_EQ_D, 
                                                    FM_EQ_D, SUBGP_EQ_D, GP_EQ_D,
                                                    SUPGP_EQ_D, SAMPLE_, path, ALTERNA, 
                                                    PARENT_, SPONSOR, MAP_SRC, MAP_WEB,
                                                    VERSION, RELEASED, LEX_RCS_I,
                                                    NOM_SCALE, BGS_REF, NOM_BGS_YR,
                                                    UUID, BGSTYPE, OPERATO, DATE_UP,
                                                    EPR_COD, PLANNIN, MPA_COD))

st_write(cadmium_subset, "/Volumes/Personal/Metals/BRITPITS/New_datasets/Cadmium_Soil_BRITPITS_Geology_Bedrock_Final.shp")
