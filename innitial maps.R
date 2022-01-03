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



# add grouping variable ---------------------------------------------------
#make a df with only county names of the main island
countynames <- tw.main.counties.sf
st_geometry(countynames) <- NULL 
countynames <- countynames[,c(1,4)]

#define north,south, east and west (N,S,E&W)
#north
N <- c("Yilan County",
        "Keelung City",
        "Taipei City", 
        "New Taipei City",
       "Taoyuan City",
       "Hsinchu City",
       "Hsinchu County"
              )
#south
S <- c("Tainan City",
       "Kaohsiung City",
       "Taitung County",
       "Pingtung County" 
       )
#east
E <- c("Nantou County",
       "Hualien County")
#west
W <- c("Yunlin County",
       "Taichung City",
       "Miaoli County",
       "Chiayi City",
       "Chiayi County",
       "Changhua County"
       )

tw.grouped <- tw.main.counties.sf
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%N] <- "N"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%S] <- "S"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%E] <- "E"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%W] <- "W"
rm(E,W,S,N)


# make a plot -------------------------------------------------------------
twmap <- function(region="N"){
            print(
            ggplot()+
              geom_sf(data=subset(tw.grouped, GROUP!=region), colour="white", size=.05, fill="#95cfc7")+
              geom_sf(data=subset(tw.grouped, GROUP==region),  colour="white", size=0,  fill="#23b09d")+
              theme(panel.grid = element_blank(),
                       axis.title = element_blank(),
                       axis.text = element_blank(),
                        axis.ticks= element_blank(),
                    panel.background = element_blank()
                    )
            )
            }

twmap("S")

