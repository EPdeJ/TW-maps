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
library(Cairo) #for PNG24


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
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%N] <- "North Taiwan"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%S] <- "South Taiwan"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%E] <- "East Taiwan"
tw.grouped$GROUP[tw.grouped$COUNTYENG%in%W] <- "West Taiwan"
rm(E,W,S,N)

tw.grouped$GROUP <- as.factor(tw.grouped$GROUP)
levels(tw.grouped$GROUP) <- list('North Taiwan'="N", 'South Taiwan'="S", 'West Taiwan'="W",'East Taiwan'="E")


# make a plot -------------------------------------------------------------
twmap <- function(region){
              ggplot()+
              geom_sf(data=subset(tw.grouped, GROUP!=region), colour="white", size=.05, fill="#95cfc7")+
              geom_sf(data=subset(tw.grouped, GROUP==region),  colour="white", size=.05,  fill="#23b09d")+
              theme(panel.grid.minor = element_blank(), 
                        panel.grid.major = element_blank(),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks= element_blank(),
                        panel.background = element_rect(fill = "transparent",color = NA), 
                        plot.background = element_rect(fill = "transparent", color = NA)
                    )
              }

# make a loop -------------------------------------------------------------
twmap("North Taiwan")



for (i in levels(tw.grouped$GROUP)){
  twmap(region=i)
  ggsave(paste0(i,"_plot.png"), type = "cairo-png", bg = "transparent")
 }

# add gpx -----------------------------------------------------------------
file_name <- "gpx/Route1_Coffee_road_.gpx"
st_layers(file_name)
gpx <- st_read(file_name,layer="track_points")

  ggplot()+
    geom_sf(data=subset(tw.grouped, GROUP!="South Taiwan"), colour="white", size=.05, fill="#95cfc7")+
    geom_sf(data=subset(tw.grouped, GROUP=="South Taiwan"),  colour="white", size=.05,  fill="#23b09d")+
    geom_sf(data=gpx, colour="red")+
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks= element_blank(),
          panel.background = element_rect(fill = "transparent",color = NA), 
          plot.background = element_rect(fill = "transparent", color = NA)
    )
gpx
summary(gpx)
