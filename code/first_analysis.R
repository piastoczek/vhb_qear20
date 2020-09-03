#########################
# Assignment No. 2
# Group 5
#########################
library(tidyverse)
library("sf")

## load insolvency dataset
setwd("C:/Users/Simone/Documents/GitHub/vhb_qear20")

insolvency_data <- read.csv("raw_data/insolvency_filings_de_julaug2020_incomplete.csv", header = TRUE, sep = ",", encoding = "UTF-8")
str(insolvency_data)

table(insolvency_data$date) #data from 01/07/2020 to 18/08/2020
table(insolvency_data$subject) #10 different factors


# first observations:
#  - one court_file_number can have multiple subjects at the same date
#  - no "Berlin" but "Charlottenburg"
#  - insolvency development over time

## Data cleaning
#write function to clean the dataset
insolvency_data_cleaning <- function(data){
  #transform date from chr to date format
  data$date <- as.Date(data$date)
  #transform insolvency_court to factor variable
  data$insolvency_court <- as.factor(data$insolvency_court)
  #transform subject to factor variable
  data$subject <- as.factor(data$subject)
  #return dataframe
  return(data)
}

#apply function
clean_insolvency_data <- insolvency_data_cleaning(insolvency_data)
str(clean_insolvency_data)

## Add map data to create insolvency map
#for downloading the map data go to https://gadm.org/download_country_v3.html search for "Germany" and save sf-files in raw_data
map_federal_states <- readRDS("raw_data/gadm36_DEU_1_sf.rds")
map_districts <- readRDS("raw_data/gadm36_DEU_2_sf.rds")
map_municipality <- readRDS("raw_data/gadm36_DEU_3_sf.rds")

#combine insolvency dataset and map data on municipality level
insolvency_per_municipality <- left_join(clean_insolvency_data, map_municipality, by = c("insolvency_court" = "NAME_3")) %>%
  select(date, insolvency_court, court_file_number, subject, name_debtor, domicile_debtor, NAME_0, NAME_1, NAME_2)

#understand structural differences
insolvency_per_municipality$dummy <- ifelse(is.na(insolvency_per_municipality$NAME_0),1,0)
na_insolvency_per_municipality <- subset(insolvency_per_municipality, insolvency_per_municipality$dummy == 1)
table(na_insolvency_per_municipality$insolvency_court)# get those insolvency_courts which do not match

#create function to include the ones that have not been found
prepare_join <- function(map){
  map$NAME_3[map$NAME_3 == c("Bad Kreuznach(Verbandsgemeinde)","Bad Kreuznach(Verbandsfreie Gemeinde)")] <- "Bad Kreuznach"
  map$NAME_3[map$NAME_3 == c("Bad Homburg v.d. Höhe")] <- "Bad Homburg v.d.Höhe"
  map$NAME_3[map$NAME_3 == c("Berlin")] <- "Charlottenburg"
  map$NAME_3[map$NAME_3 == c("Esslingen am Neckar")] <- "Esslingen"
  map$NAME_3[map$NAME_3 == c("Frankfurt (Oder)")] <- "Frankfurt/Oder"
  map$NAME_3[map$NAME_3 == c("Freiburg im Breisgau")] <- "Freiburg"
  map$NAME_3[map$NAME_3 == c("Kempten (Allgäu)")] <- "Kempten"
  map$NAME_3[map$NAME_3 == c("Königstein im Taunus")] <- "Königstein/Ts."
  map$NAME_3[map$NAME_3 == c("Leer (Ostfriesland)")] <- "Leer"
  map$NAME_3[map$NAME_3 == c("Limburg a.d. Lahn")] <- "Limburg"
  map$NAME_3[map$NAME_3 == c("Ludwigshafen am Rhein")] <- "Ludwigshafen/Rhein"
  map$NAME_3[map$NAME_3 == c("Marburg")] <- "Marburg/Lahn"
  map$NAME_3[map$NAME_3 == c("Mühldorf a. Inn")] <- "Mühldorf"
  map$NAME_3[map$NAME_3 == c("Neustadt an der Weinstraße")] <- "Neustadt a. d. Wstr."
  map$NAME_3[map$NAME_3 == c("Weiden i.d. OPf.")] <- "Weiden"
  map$NAME_3[map$NAME_3 == c("Weilheim i. OB")] <- "Weilheim"
  
  # no assignment possible 
  #map$NAME_3[map$NAME_3 == c("")] <- "Niebüll"
  #map$NAME_3[map$NAME_3 == c("")] <- "Meldorf"
  #map$NAME_3[map$NAME_3 == c("")] <- "Ravensburg"
  
  return(map)
}

#apply function
clean_map_municipality <- prepare_join(map_municipality)
#join cleaned version again with insolvency data
clean_insolvency_data <- left_join(clean_insolvency_data, clean_map_municipality, by = c("insolvency_court" = "NAME_3")) %>%
  select(date, insolvency_court, court_file_number, subject, name_debtor, domicile_debtor, NAME_0, NAME_1, NAME_2)

##create some tables with aggregated data
summary_table_3 <- summarytools::freq(clean_insolvency_data$NAME_1, order = "freq")
table_4 <- as.data.frame(table(clean_insolvency_data$NAME_1, clean_insolvency_data$subject))

##create insolvency map
count_insolvency <- insolvency_data %>%
   group_by(insolvency_court)%>%
   count()

#join datasets ( 
map <- left_join(clean_municipality, count_insolvency, by = c("NAME_3" = "insolvency_court")) %>% 
# Shouldn't it be "clean_map_municipality" (83) inestad of "clean_municipality"?
  select(NAME_3, n, geometry)

#transform map
object <- st_as_sf(map)

#create map
tm_shape(object) +
  tm_polygons("n")

#create interactiv map
tmap_mode("view")
tm_basemap("OpenStreetMap.DE")+
  tm_shape(object) +
  tm_bubbles("n", col = "red")


