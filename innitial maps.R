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
library(ggspatial)

# get/set workspace -----------------------------------------------------------
getwd()

# load shapefile ----------------------------------------------------------
tw.counties.sf <- st_read("GIS/twshape/mapdata202008310842/COUNTY_MOI_1090820.shp") #load the downloaded counties as sf

# make df without geom ----------------------------------------------------
tw.counties.df <-  tw.counties.sf #copy sf
st_geometry(tw.counties.df) <- NULL #set geom to NULL to get dataframe

# get tw outline ----------------------------------------------------------
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

# make county selection ---------------------------------------------------
county <- c("A","F","C")
tw.county.select <- filter(tw.main.counties.sf,COUNTYID%in%county)
tw.county.nonselect <- filter(tw.main.counties.sf,COUNTYID!=county)

# add grouping variable ---------------------------------------------------
#make a df with only county names of the main island
countynames <- tw.main.counties.sf
st_geometry(countynames) <- NULL 
countynames <- countynames[,c(1,4)]

#define north,south, east and west (N,S,E&W)
N <- c("A,F,C,G")
S <- 
E <- 
W <- 

tw.grouped <- tw.main.counties.sf


test$GROUP[test$COUNTYID%in%c("E","T")] <- "S"

head(test[20,])


# make a plot -------------------------------------------------------------
ggplot()+
  geom_sf(data=tw.county.nonselect, colour="white", size=.05, fill="#95cfc7")+
  geom_sf(data=tw.county.select,  size=0,  fill="#23b09d")+
  theme(panel.grid = element_blank(),
           axis.title = element_blank(),
           axis.text = element_blank(),
            axis.ticks= element_blank(),
        panel.background = element_blank()
           )

