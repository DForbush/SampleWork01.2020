#This script was used to generate maps and summary statistics for the analysis of 2012-2016 CTPP data and 2006-2010 CTPP data.
#These datasets survey mode choice to analyze how people are getting to work
#The source of the raw datasets are the U.S. Census Bureau American Community Survey

#R Script Author: Dan Forbush, 2019 Mayoral Fellow

#Set WD
#setwd() - Actual directory has been redacted for security purposes.

#load libraries
library(dplyr)
library(tidyr)
library(sp)
library(sf)
library(tmap)

#load data
Mode_2012_2016 <- read.csv(Mode_Data_CTPP_2012_2016.csv)

#Remove commas and convert some classes
Mode_2012_2016$Workers_16_and_Over <- sub(",", "", Mode_2012_2016$Workers_16_and_Over)
Mode_2012_2016$Workers_16_and_Over <- as.numeric(as.character(Mode_2012_2016$Workers_16_and_Over))
Mode_2012_2016$RESIDENCE <- as.character(Mode_2012_2016$RESIDENCE)

#rearrange dataset and do some column renaming
Mode_Long <- Mode_2012_2016 %>% spread(key = Means_of_Transportation, value = Workers_16_and_Over)
colnames(Mode_Long)[colnames(Mode_Long)=="Bus or trolley bus"] <- "Bus"
colnames(Mode_Long)[colnames(Mode_Long)=="Car, truck, or van -- Drove alone"] <- "Car_drove_alone"
colnames(Mode_Long)[colnames(Mode_Long)=="Car, truck, or van -- In a 2-person carpool"] <- "Car_2_person_carpool"
colnames(Mode_Long)[colnames(Mode_Long)=="Car, truck, or van -- In a 3-person carpool"] <- "Car_3_person_carpool"
colnames(Mode_Long)[colnames(Mode_Long)=="Subway or elevated"] <- "Subway_or_Elevated"
colnames(Mode_Long)[colnames(Mode_Long)=="Total, means of transportation"] <- "Total"

#sum up modes of transit by residence census tract, eliminating workplace destination
Mode_by_residence <- Mode_Long %>% 
  replace(is.na(.), 0) %>%
  group_by(RESIDENCE) %>% summarize(sumCar = sum(Auto), sumBike = sum(Bicycle),
                                    sumBus = sum(Bus), sumRailroad = sum(Railroad),
                                    sumEl = sum(Subway_or_Elevated), sumTotal = sum(Total),
                                    sumWalked = sum(Walked))

#create columns for percentages who take each mode of transit
Mode_by_residence$Pct_Car <- (Mode_by_residence$sumCar/Mode_by_residence$sumTotal)*100
Mode_by_residence$Pct_El <- (Mode_by_residence$sumEl/Mode_by_residence$sumTotal)*100
Mode_by_residence$Pct_Bus <- (Mode_by_residence$sumBus/Mode_by_residence$sumTotal)*100
Mode_by_residence$Pct_Bicycle <- (Mode_by_residence$sumBike/Mode_by_residence$sumTotal)*100
Mode_by_residence$Pct_Walking <- (Mode_by_residence$sumWalked/Mode_by_residence$sumTotal)*100
Mode_by_residence$Pct_Railroad <- (Mode_by_residence$sumRailroad/Mode_by_residence$sumTotal)*100

#need to remove a common phrase for the merges
Mode_by_residence$RESIDENCE <- sub(", Cook County, Illinois", "", Mode_by_residence$RESIDENCE)

#load shapefiles
#Chicago census tracks
chi <- st_read("geo_export_64d368e3-b16e-47f3-838b-aff906a01d6f.shp")

#eTOD shapefiles
Metra_no_transit <- st_as_sf(st_read("METRA__radius_(No_Transit_Locations).shp"))
Metra_transit <- st_as_sf(st_read("METRA_radius_(Transit_Locations).shp"))
CTA_no_transit <- st_as_sf(st_read("CTA_radius_(No_Transit_Locations).shp"))
CTA_transit <- st_as_sf(st_read("CTA_radius_(Transit_Locations).shp"))

#eTOD shapefiles updated 10.1.19
Metra_no_transit <- st_as_sf(st_read("Metra_Buffers_(No_Transit_Locations)_Dissolve.shp"))
Metra_transit <- st_as_sf(st_read("Metra_Buffers_(Transit_Locations)_Dissolve.shp"))
CTA_no_transit <- st_as_sf(st_read("CTA_Buffers_(No_Transit_Locations)_Dissolve.shp"))
CTA_transit <- st_as_sf(st_read("CTA_Buffers_(Transit_Locations)_Dissolve.shp"))

#Merge dataframe with Chicago census tract shapefile and convert to sf object
chi_df <- sp::merge(y = chi, x = Mode_by_residence, by.y = 'namelsad10', by.x = 'RESIDENCE')
chi_sf <- st_as_sf(chi_df)


#Make the maps. Use tmap_mode("view") for interactive maps. 
tmap_mode("plot")
#start with CTA
#set breaks for each map
carbreaks <- c(0, seq(from = 20, to = 80, by = 5), 100)
elbreaks <- c(seq(from = 0, to = 40, by = 4), 50, 60, 100)
busbreaks <- c(seq(from = 0, to = 20, by = 2), seq(from = 24, to = 40, by = 4), 50, 60, 100)
railbreaks <- c(seq(from = 0, to = 5, by = 1), seq(from = 6, to = 30, by = 4), 50, 100)
bikebreaks <- c(seq(from = 0, to = 5, by = 1), seq(from = 6, to = 25, by = 3), 50, 100)
walkbreaks <- c(seq(from = 0, to = 20, by = 2), seq(from = 24, to = 40, by = 4), 50, 60, 100)

carcta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Car", palette = "Greys", breaks = carbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Car")

elcta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_El", palette = "Greys", breaks = elbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by El")

buscta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Bus", palette = "Greys", breaks = busbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bus")

railcta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Railroad", palette = "Greys", breaks = railbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Rail")

bikecta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Bicycle", palette = "Greys", breaks = bikebreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bicycle")

walkcta <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Walking", palette = "Greys", breaks = walkbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Walking")

tmap_arrange(carcta, elcta, buscta, railcta, bikecta, walkcta, ncol = 3, nrow = 2)

#next, do Metra
carm <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Car", palette = "Greys", breaks = carbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Car")

elm <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_El", palette = "Greys", breaks = elbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by El")

busm <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Bus", palette = "Greys", breaks = busbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bus")

railm <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Railroad", palette = "Greys", breaks = railbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Rail")

bikem <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Bicycle", palette = "Greys", breaks = bikebreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bicycle")

walkm <- tm_shape(chi_sf, is.master = TRUE) +
  tm_polygons(col = "Pct_Walking", palette = "Greys", breaks = walkbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Walking")

tmap_arrange(carm, elm, busm, railm, bikem, walkm, ncol = 3, nrow = 2)

#repeat with 2006-2010 Data
Mode_2006_2010 <- read.csv("Mode_Data_CTPP_2006_2010.csv")

#Remove commas and convert some classes
Mode_2006_2010$Workers_16_and_Over <- sub(",", "", Mode_2006_2010$Workers_16_and_Over)
Mode_2006_2010$Workers_16_and_Over <- as.numeric(as.character(Mode_2006_2010$Workers_16_and_Over))
Mode_2006_2010$RESIDENCE <- as.character(Mode_2006_2010$RESIDENCE)

#rearrange dataset and do some column renaming
Mode_Long_old <- Mode_2006_2010 %>% spread(key = Means_of_Transportation, value = Workers_16_and_Over)
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Bus or trolley bus"] <- "Bus"
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Car, truck, or van -- Drove alone"] <- "Car_drove_alone"
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Car, truck, or van -- In a 2-person carpool"] <- "Car_2_person_carpool"
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Car, truck, or van -- In a 3-person carpool"] <- "Car_3_person_carpool"
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Subway or elevated"] <- "Subway_or_Elevated"
colnames(Mode_Long_old)[colnames(Mode_Long_old)=="Total, means of transportation"] <- "Total"

#sum up modes of transit by residence census tract, eliminating workplace destination
Mode_by_residence_old <- Mode_Long_old %>% 
  replace(is.na(.), 0) %>%
  group_by(RESIDENCE) %>% summarize(sumCar = sum(Car_drove_alone, Car_2_person_carpool, Car_3_person_carpool), 
                                    sumBike = sum(Bicycle),
                                    sumBus = sum(Bus), sumRailroad = sum(Railroad),
                                    sumEl = sum(Subway_or_Elevated), sumTotal = sum(Total),
                                    sumWalked = sum(Walked))

#create columns for percentages who take each mode of transit
Mode_by_residence_old$Pct_Car <- (Mode_by_residence_old$sumCar/Mode_by_residence_old$sumTotal)*100
Mode_by_residence_old$Pct_El <- (Mode_by_residence_old$sumEl/Mode_by_residence_old$sumTotal)*100
Mode_by_residence_old$Pct_Bus <- (Mode_by_residence_old$sumBus/Mode_by_residence_old$sumTotal)*100
Mode_by_residence_old$Pct_Bicycle <- (Mode_by_residence_old$sumBike/Mode_by_residence_old$sumTotal)*100
Mode_by_residence_old$Pct_Walking <- (Mode_by_residence_old$sumWalked/Mode_by_residence_old$sumTotal)*100
Mode_by_residence_old$Pct_Railroad <- (Mode_by_residence_old$sumRailroad/Mode_by_residence_old$sumTotal)*100

#need to remove a common phrase for the merges
Mode_by_residence_old$RESIDENCE <- sub(", Cook County, Illinois", "", Mode_by_residence_old$RESIDENCE)

#Merge dataframe with Chicago census tract shapefile and convert to sf object
chi_df_old <- sp::merge(y = chi, x = Mode_by_residence_old, by.y = 'namelsad10', by.x = 'RESIDENCE')
chi_sf_old <- st_as_sf(chi_df_old)

#start with CTA
carctao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Car", palette = "Greys", breaks = carbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Car")

elctao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_El", palette = "Greys", breaks = elbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by El")

busctao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Bus", palette = "Greys", breaks = busbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bus")

railctao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Railroad", palette = "Greys", breaks = railbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Rail")

bikectao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Bicycle", palette = "Greys", breaks = bikebreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bicycle")

walkctao <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Walking", palette = "Greys", breaks = walkbreaks) +
  tm_shape(CTA_no_transit) + tm_borders(col = "palegreen", alpha = 1, lwd = 2) +
  tm_shape(CTA_transit) + tm_borders(col = "darkgreen", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Walking")

tmap_arrange(carctao, elctao, busctao, railctao, bikectao, walkctao, ncol = 3, nrow = 2)

#next, do Metra
carmo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Car", palette = "Greys", breaks = carbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Car")

elmo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_El", palette = "Greys", breaks = elbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by El")

busmo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Bus", palette = "Greys", breaks = busbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bus")

railmo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Railroad", palette = "Greys", breaks = railbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Rail")

bikemo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Bicycle", palette = "Greys", breaks= bikebreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Bicycle")

walkmo <- tm_shape(chi_sf_old, is.master = TRUE) +
  tm_polygons(col = "Pct_Walking", palette = "Greys", breaks = walkbreaks) +
  tm_shape(Metra_no_transit) + tm_borders(col = "cadetblue1", alpha = 1, lwd = 2) +
  tm_shape(Metra_transit) + tm_borders(col = "blue", alpha = 1, lwd = 2) +
  tm_layout(main.title = "% Who Commute by Walking")

tmap_arrange(carmo, elmo, busmo, railmo, bikemo, walkmo, ncol = 3, nrow = 2)

#some summary statistics
colMeans(chi_df[sapply(chi_df, is.numeric)])
colMeans(chi_df_old[sapply(chi_df_old, is.numeric)])

library(psych)
indescribe(chi_df[sapply(chi_df, is.numeric)])
describe(chi_df_old[sapply(chi_df_old, is.numeric)])


#Come up with mean mode choice percentages for CTA TOD vs. CTA non-TOD
#Start with 2012-2016 data
CTA_transit_trans <- st_transform(CTA_transit, crs = 4326)
chi_sf_trans <- st_transform(chi_sf, crs = 4326)
CTA_TOD_overlap <- st_intersects(st_centroid(chi_sf_trans), CTA_transit_trans)
chi_sf_CTATOD <- chi_sf_trans[lengths(CTA_TOD_overlap)>0, ]
chi_df_CTATOD <- as.data.frame(chi_sf_CTATOD)
colMeans(chi_df_CTATOD[sapply(chi_df_CTATOD, is.numeric)], na.rm = TRUE)

CTA_no_transit_trans <- st_transform(CTA_no_transit, crs = 4326)
CTA_no_TOD_overlap <- st_intersects(st_centroid(chi_sf_trans), CTA_no_transit_trans)
chi_sf_CTAnoTOD <- chi_sf_trans[lengths(CTA_no_TOD_overlap)>0, ]
chi_df_CTAnoTOD <- as.data.frame(chi_sf_CTAnoTOD)
colMeans(chi_df_CTAnoTOD[sapply(chi_df_CTAnoTOD, is.numeric)], na.rm = TRUE)

#Repeat with 2006-2010 data. 
#Note: this code will overwrite variables created in the 12 preceding lines of code
chi_sf_old_trans <- st_transform(chi_sf_old, crs = 4326)
CTA_TOD_overlap <- st_intersects(st_centroid(chi_sf_old_trans), CTA_transit_trans)
chi_sf_CTATOD <- chi_sf_trans[lengths(CTA_TOD_overlap)>0, ]
chi_df_CTATOD <- as.data.frame(chi_sf_CTATOD)
colMeans(chi_df_CTATOD[sapply(chi_df_CTATOD, is.numeric)], na.rm = TRUE)

CTA_no_TOD_overlap <- st_intersects(st_centroid(chi_sf_old_trans), CTA_no_transit_trans)
chi_sf_CTAnoTOD <- chi_sf_trans[lengths(CTA_no_TOD_overlap)>0, ]
chi_df_CTAnoTOD <- as.data.frame(chi_sf_CTAnoTOD)
colMeans(chi_df_CTAnoTOD[sapply(chi_df_CTAnoTOD, is.numeric)], na.rm = TRUE)
