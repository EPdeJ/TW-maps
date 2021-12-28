# Explainer ---------------------------------------------------------------
#'This r script loads shape files from Taiwan and maps them, to highlight
#'counties. All shapefiles used can be downloaded from:
#'https://whgis.nlsc.gov.tw/English/5-1Files.aspx or
#'https://data.gov.tw/dataset/7442

# Load packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(leaflet)
library(raster)

# get/set workspace -----------------------------------------------------------
getwd()

# load shapefile ----------------------------------------------------------
tw.counties.sf <- st_read("GIS/twshape/mapdata202008310842/COUNTY_MOI_1090820.shp") #load the downloaded counties as sf

# make df without geom ----------------------------------------------------
tw.counties.df <-  tw.counties.sf #copy sf
st_geometry(tw.counties.df) <- NULL #set geom to NULL to get dataframe

# get tw outline ----------------------------------------------------------
tw = getData('GADM', country='TW', level=0) 
tw <- st_as_sf(tw)
tw.boundry = getData('GADM', country='TW', level=0) %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1) #get outer line 

st_crs(tw.boundry)
tw.boundry <- st_transform(tw.boundry,crs = 3824)


# crop in on tw and remove islands-----------------------------------------------------------
tw.main.counties.sf <- st_intersection(tw.counties.sf,tw.boundry)

# make a plot -------------------------------------------------------------
ggplot(data=tw)+
  geom_sf()+
  theme_bw()
st_bbox(tw.counties.sf.croped)


