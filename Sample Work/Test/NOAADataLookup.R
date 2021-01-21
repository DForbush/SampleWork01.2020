#load libraries
library(rstudioapi)
library(dplyr)
library(tidyr)
library(openxlsx)
library(psych)
library(sf)
library(sp)
library(data.table)

#SetWD
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)
print(getwd())

print("Hello!")

#load data - tornados - includes files from the X-drive that were downloaded by Yingfei (I think)
#I will limit data to 2008 and later so we only have to worry about the EF scale and not the F scale
options(scipen = 99999)
tornado1 <- read.csv("storm_data_search_results.csv_2015-2020.txt", stringsAsFactors=FALSE)
tornado2 <- read.csv("storm_data_search_results.csv_2010-2015.txt", stringsAsFactors=FALSE)
tornado3 <- read.csv("storm_data_search_results_2005-2010.csv.txt", stringsAsFactors=FALSE)
tornado4 <- read.csv("storm_data_search_results_2000-2005.csv.txt", stringsAsFactors=FALSE)
tornado5 <- read.csv("storm_data_search_results_1990-2000.txt", stringsAsFactors=FALSE)
tornado6 <- read.csv("storm_data_search_results_1980-1990.csv.txt", stringsAsFactors=FALSE)
tornado7 <- read.csv("storm_data_search_results_1970-1980.csv.txt", stringsAsFactors=FALSE)
tornado8 <- read.csv("storm_data_search_results_1952-1970.csv.txt", stringsAsFactors=FALSE)

tornado <- rbind(tornado1, tornado2, tornado3, tornado4, tornado5, tornado6, tornado7, tornado8)
rm(tornado1, tornado2, tornado3, tornado4, tornado5, tornado6, tornado7, tornado8)

#there appears to be two rows that are corrupted in the uploaded files. I'll remove them
tornado <- tornado[tornado$EVENT_ID!= "10332982", ]
tornado <- tornado[tornado$EVENT_ID!= "151", ]

tornado$YEAR <- as.numeric(substring(tornado$BEGIN_DATE, 7, 10))

#tornado table for the report - will only show years 2010 and later
tornadoshort <- tornado[tornado$YEAR >= 2010, ]
table1 <- tornadoshort %>% group_by(YEAR, TOR_F_SCALE) %>% summarise(count = n()) %>% pivot_wider(names_from = YEAR, values_from = count)
n <- table1$TOR_F_SCALE
table1.5 <- as.data.frame(t(table1[, -1]))
colnames(table1.5) <- n

tornado$DEATHS_DIRECT <- as.numeric(tornado$DEATHS_DIRECT)

table2 <- tornadoshort %>% select(YEAR, DEATHS_DIRECT, INJURIES_DIRECT, DEATHS_INDIRECT, INJURIES_INDIRECT) %>% 
  group_by(YEAR) %>% 
  summarise(deaths = sum(DEATHS_DIRECT + DEATHS_INDIRECT, na.rm = TRUE), injuries = sum(INJURIES_DIRECT + INJURIES_INDIRECT, na.rm = TRUE))

tornadotable <- cbind(table1.5, table2[, 2:3])

#summarize by County
table3 <- tornado %>% group_by(CZ_NAME_STR) %>% summarise(Tornado_count = n())
#annoyingly, there are some "merged" counties. I will add these each to their original and get rid of them
dup <- c("CLAIBORNE CO.", "COPIAH CO.", "TUNICA CO.", "DESOTO CO.", "WALTHALL CO.", "MARION CO.", "NESHOBA CO.", "KEMPER CO.", "ISSAQUENA CO.", "SHARKEY CO.",
         "YAZOO CO.", "HOLMES CO.", "WARREN CO.", "HINDS CO.", "RANKIN CO.", "SCOTT CO.")
dup2 <- c("CLAIBORNE CO.", "RANKIN CO.") #these show up twice

table3$Tornado_count[table3$CZ_NAME_STR %in% dup] <- table3$Tornado_count[table3$CZ_NAME_STR %in% dup] + 1
table3$Tornado_count[table3$CZ_NAME_STR %in% dup2] <- table3$Tornado_count[table3$CZ_NAME_STR %in% dup2] + 1

table3 <- table3[-grep("AND", table3$CZ_NAME_STR),]

table3$CZ_NAME_STR <- gsub(" CO.", "", table3$CZ_NAME_STR)
table3$CZ_NAME_STR <- tolower(table3$CZ_NAME_STR)
#the following comes from the ?tolower documentation
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
table3$CZ_NAME_STR <-capwords(table3$CZ_NAME_STR)

#add in tornado risk column ranked 3 - 5 per the exposure level detailed in the analysis. 
table3$TorRisk[table3$Tornado_count <= 35] <- 3 #moderate exposure
table3$TorRisk[table3$Tornado_count > 35 & table3$Tornado_count <= 65] <- 4 #moderate-high exposure
table3$TorRisk[table3$Tornado_count > 65] <- 5 #high exposure

#load data - winter storms - note, in winterstorms3 I had to get rid of the event narrative. It was screwing up the data import. 
winterstorms1 <- read.csv("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/NOAA Data/winter_storms_2015_to_feb2020.csv", stringsAsFactors=FALSE)
winterstorms2 <- read.csv("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/NOAA Data/winter_storms_2010_to_2014.csv", stringsAsFactors=FALSE)
winterstorms3 <- read.csv("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/NOAA Data/winter_storms_2000_to_2009.csv", stringsAsFactors=FALSE)
winterstorms4 <- read.csv("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/NOAA Data/winter_storms_1996_to_1999.csv", stringsAsFactors=FALSE)
winterstorms <- rbind(winterstorms1, winterstorms2, winterstorms3, winterstorms4)
rm(winterstorms1, winterstorms2, winterstorms3, winterstorms4)

#summarize by County and merge it with the previously loaded shapefile - this is much easier than with tornados
table4 <- winterstorms %>% group_by(CZ_NAME_STR) %>% summarise(winterstorm_count = n())
table4$CZ_NAME_STR <- gsub(" \\(ZONE\\)", "", table4$CZ_NAME_STR)
table4$CZ_NAME_STR <- tolower(table4$CZ_NAME_STR)
table4$CZ_NAME_STR <-capwords(table4$CZ_NAME_STR)

#Add in column for winter storm risk to reflect what is in the analysis
table4$WtrRisk[table4$winterstorm_count <= 14] <- 1
table4$WtrRisk[table4$winterstorm_count > 14 & table4$winterstorm_count <= 20] <- 2
table4$WtrRisk[table4$winterstorm_count > 20 & table4$winterstorm_count <= 25] <- 3
table4$WtrRisk[table4$winterstorm_count > 25 & table4$winterstorm_count <= 30] <- 4
table4$WtrRisk[table4$winterstorm_count > 30] <- 5

shape3 <- sp::merge(x = shape2, y = table4, by.x = "NAME", by.y = "CZ_NAME_STR")


#Winter storm table for the report - NOTE: NEED TO UPDATE THIS CODE IF I'M GOING TO MAKE CHANGES TO THE TABLE ALREADY IN WORD
winterstorms$YEAR <- as.numeric(substring(winterstorms$BEGIN_DATE, 7, 10))

winter <- winterstorms %>% select(BEGIN_DATE, EVENT_TYPE, YEAR, DAMAGE_PROPERTY_NUM, DEATHS_DIRECT, INJURIES_DIRECT, DEATHS_INDIRECT, INJURIES_INDIRECT) %>% 
  group_by(BEGIN_DATE, EVENT_TYPE, YEAR) %>% 
  summarise(property_damage = sum(DAMAGE_PROPERTY_NUM, na.rm = TRUE), deaths = sum(DEATHS_DIRECT + DEATHS_INDIRECT, na.rm = TRUE), injuries = sum(INJURIES_DIRECT + INJURIES_INDIRECT, na.rm = TRUE))

winter$property_damage <- as.numeric(winter$property_damage)

wintertable1 <- table(winter$YEAR, winter$EVENT_TYPE)
wintertable2 <- aggregate(winter$property_damage, by = list(winter$YEAR), FUN = sum)
wintertable3 <- aggregate(winter$deaths, by = list(winter$YEAR), FUN = sum)
wintertable4 <- aggregate(winter$injuries, by = list(winter$YEAR), FUN = sum)
wintertable <- cbind(wintertable1, property_damage = wintertable2$x, deaths = wintertable3$x, injuries = wintertable4$x)
write.table(wintertable, file = "winterstormtable.csv")

#also get some statistics...
plot(density(shape3$Tornado_count))
quantile(shape3$Tornado_count, probs = seq(0, 1, 0.2))

#Now do Hurricane Data - this does not provide much information...
hurricane <- read.csv("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/GIS/Data/Hurricane/storm_data_search_results.csv.txt", stringsAsFactors=FALSE)

#lets try an R package
library(hurricaneexposure)
library(hurricaneexposuredata)

data("rain")
head(rain)

MScounties <- read.xlsx("C:/Users/dforbush/OneDrive - Cambridge Systematics/Mississippi LRTP/GIS/Mississippi_Counties.xlsx")
MScounties$FIPS <- as.character(MScounties$FIPS)
MScountiesfips <- MScounties$FIPS

MSrain <- county_rain(counties = MScountiesfips, start_year = 1988, end_year = 2018, rain_limit = 50, dist_limit = 1000, days_included = c(-2, -1, 0, 1, 2))
MSwind <- county_wind(counties = MScountiesfips, start_year = 1988, end_year = 2018, wind_limit = 11.176, wind_var = "vmax_sust")

#convert wind speed from m/s to mph and rain accumulation from mm to inches - don't run these lines multiple times!
MSwind$vmax_sust <- MSwind$vmax_sust * 2.23694
MSwind$vmax_gust <- MSwind$vmax_gust * 2.23694
MSrain$tot_precip <- MSrain$tot_precip * 0.0393701


#get the max precipitation, max wind speed (both gust and sustained)
table5 <- MSrain %>% group_by(fips) %>% summarise(max.precip = max(tot_precip))
table6 <- MSwind %>% group_by(fips) %>% summarise(max.wind = max(vmax_sust), max.gust = max(vmax_gust))
table7 <- merge(table5, table6)

#merge in county names and then merge to the Counties shapefile, loaded above
table8 <- merge(table7, MScounties, by.x = "fips", by.y = "FIPS")

#add in hurricane risk column based on 
table8$HurRisk[table8$max.gust <= 55] <- 1
table8$HurRisk[table8$max.gust > 55 & table8$max.gust <= 70] <- 2
table8$HurRisk[table8$max.gust > 70 & table8$max.gust <= 85] <- 3
table8$HurRisk[table8$max.gust > 85 & table8$max.gust <= 100] <- 4
table8$HurRisk[table8$max.gust > 100] <- 5