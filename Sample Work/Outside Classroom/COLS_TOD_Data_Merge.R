#This script will limit the properties listed in the City-Owned Land System to those that are within a 1/2 mile radious 
#of either a CTA or Metra station and thus eligible for TOD zoning incentives. 

#Set wd
#setwd("") #redacted for privacy purposes

#load libraries
library(sf)
library(sp)
library(tmap)
library(dplyr)

#load data
COLS <- read.csv("City-Owned_Land_Inventory.csv")

#load shapefiles
Metra_no_TOD <- st_as_sf(st_read("METRA__radius_(No_Transit_Locations).shp"))
Metra_TOD <- st_as_sf(st_read("METRA_radius_(Transit_Locations).shp"))
CTA_no_TOD <- st_as_sf(st_read("CTA_radius_(No_Transit_Locations).shp"))
CTA_TOD <- st_as_sf(st_read("CTA_radius_(Transit_Locations).shp"))

#need to set coordinates of the shape files
Metra_no_TOD <- st_transform(Metra_no_TOD, crs = 4326)
Metra_TOD <- st_transform(Metra_TOD, crs = 4326)
CTA_no_TOD <- st_transform(CTA_no_TOD, crs = 4326)
CTA_TOD <- st_transform(CTA_TOD, crs = 4326)

#need to remove NAs and examples where latitutde and longitude = 0 
COLS <- COLS[!is.na(COLS$Latitude) | !is.na(COLS$Longitude), ]
COLS <- COLS[COLS$Latitude != 0 | COLS$Longitude != 0, ]

#convert dataset to SF
COLS_sf <- st_as_sf(COLS, coords = c("Longitude", "Latitude"), crs = 4326)

#plot this with TOD areas
COLS_TOD_map1 <- tm_shape(COLS_sf) + tm_dots() +
  tm_shape(Metra_no_TOD) + tm_borders(col = "cadetblue1") +
  tm_shape(Metra_TOD) + tm_borders(col="blue") +
  tm_shape(CTA_no_TOD) + tm_borders(col = "palegreen") +
  tm_shape(CTA_TOD) + tm_borders(col = "darkgreen")

#NOTE: the following code repeats the object COLS_TOD_overlap. Be careful when running.
#start with overlap with Metra_no_TOD
COLS_TOD_overlap <- st_intersects(COLS_sf, Metra_no_TOD)
COLS_Metra_no_TOD <- COLS_sf[lengths(COLS_TOD_overlap)>0, ]

#Then metra TOD
COLS_TOD_overlap <- st_intersects(COLS_sf, Metra_TOD)
COLS_Metra_TOD <- COLS_sf[lengths(COLS_TOD_overlap)>0, ]

#Then CTA_no_TOD
COLS_TOD_overlap <- st_intersects(COLS_sf, CTA_no_TOD)
COLS_CTA_no_TOD <- COLS_sf[lengths(COLS_TOD_overlap)>0, ]

#Then CTA_TOD
COLS_TOD_overlap <- st_intersects(COLS_sf, CTA_TOD)
COLS_CTA_TOD <- COLS_sf[lengths(COLS_TOD_overlap)>0, ]

rm(COLS_TOD_overlap)

#map this data
COLS_TOD_map2 <- tm_shape(COLS_Metra_no_TOD) + tm_dots(col = "blue") +
  tm_shape(COLS_Metra_TOD) + tm_dots(col = "green") +
  tm_shape(COLS_CTA_no_TOD) + tm_dots(col = "yellow") +
  tm_shape(COLS_CTA_TOD) + tm_dots(col = "red") + 
  tm_shape(Metra_no_TOD) + tm_borders(col = "cadetblue1") +
  tm_shape(Metra_TOD) + tm_borders(col="blue") +
  tm_shape(CTA_no_TOD) + tm_borders(col = "palegreen") +
  tm_shape(CTA_TOD) + tm_borders(col = "darkgreen")
  
#Merge datasets
COLS_TOD_Intersect <- rbind(as.data.frame(COLS_Metra_no_TOD), as.data.frame(COLS_Metra_TOD), as.data.frame(COLS_CTA_no_TOD), as.data.frame(COLS_CTA_TOD))

#remove duplicates
COLS_TOD_Intersect <- distinct(COLS_TOD_Intersect)

#plot this dataset alone
COLS_TOD_Intersect_sf <- st_as_sf(COLS_TOD_Intersect, crs = 4326)

COLS_TOD_map3 <- tm_shape(COLS_TOD_Intersect_sf) + 
  tm_dots(col = "Property.Status", popup.vars = c("ID", "PIN", "Address", "Property.Status", "Zoning.Classification", "Last.Update")) + 
  tm_shape(Metra_no_TOD) + tm_borders(col = "cadetblue1") +
  tm_shape(Metra_TOD) + tm_borders(col="blue") +
  tm_shape(CTA_no_TOD) + tm_borders(col = "palegreen") +
  tm_shape(CTA_TOD) + tm_borders(col = "darkgreen")

#export this map as a Widget
tmap_save(tm = COLS_TOD_map3, filename = "COLS_TOD_Map.html")

#sort dataset by "owned by City"
COLS_TOD_Owned <- COLS_TOD_Intersect %>% filter(Property.Status == "Owned by City")
