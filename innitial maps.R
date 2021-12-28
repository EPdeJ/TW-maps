# Explainer ---------------------------------------------------------------
#'This r script loads shape files from Taiwan and maps them, to highlight
#'counties. All shapefiles used can be downloaded from:
#'https://whgis.nlsc.gov.tw/English/5-1Files.aspx

# Load packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(leaflet)


# load shapefile ----------------------------------------------------------
tw.layer.all <- st_read("G:/My Drive/!!!_NTU Public Health/fietsboek/GIS/counties/COUNTY_MOI_1090820.gml",
                        crs=4326,
                        stringsAsFactors=FALSE,
                        options = "ENCODING=UTF8")
st_layers("G:/My Drive/!!!_NTU Public Health/fietsboek/GIS/counties/COUNTY_MOI_1090820.gml")

# make a plot -------------------------------------------------------------
ggplot(data=tw.layer.all)+
  geom_sf()+
  theme_bw()


