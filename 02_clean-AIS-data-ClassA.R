#02_clean-AIS-data-ClassA

#load packages
library(stringr)
library(dplyr)
#load libraries
library(pacman)
p_load(ggplot2)
p_load(sp)
p_load(rgdal)
p_load(maps)
p_load(maptools)
p_load(rgeos)

#load combined raw SBARC data
readRDS("01_raw_SBARC_AIS_data_201804-combined.Rds")

##Clean Jan 2018 data
# 1. remove extra characters from the date column (which is based on the file name) to just get the date
data_rbind$`AIS_files[i]` <-
  str_sub(data_rbind$`AIS_files[i]`,-13,-8)
#2. add 20 infront of the 18 to get the full year so it meets ISO standards
data_rbind$`AIS_files[i]` <-
  sub("^", "20", data_rbind$`AIS_files[i]`)
#3. read it as a date to format it properly
data_rbind$`AIS_files[i]` <-
  as.Date(data_rbind$`AIS_files[i]`, "%Y%m%d")
#4. then make it back to a charcter because otherwise it won't concatenate properly w/the time column
data_rbind$`AIS_files[i]` <- as.character(data_rbind$`AIS_files[i]`)
#5. I don't remember what this does, I think it just makes sure all of the time fields are the proper length
data_rbind$V.1 <- str_sub(data_rbind$V.1, 1, 8)
#6. combine the date and time column together
data_rbind$datetime <-
  paste(data_rbind$`AIS_files[i]`, data_rbind$V.1, sep = " ")
#7. turn it into a POSICct (datetime) class - essentially so R recognizes it as a date. For whatever reason, even though it was already formatted to the ISO standard, it still wasn't reading it properly
#so I had to specify the format
data_rbind$datetime <-
  as.POSIXct(data_rbind$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
#8. Create column with date time converted to PST 
data_rbind$datetime_PST <- lubridate::with_tz(data_rbind$datetime,  "US/Pacific")

##Get only Class A position messages (based on format of dataframe)
#get Class A locations
ClassA_loc <- data_rbind %>%
  filter(grepl("[A-Z]", V.2) &  #this essentially uses general regional expressions to filter just for rows with letters in the column (ie just boat names)
           V.6 != 18 & #get rid of Class B AIS position report (this is the column that for Class B position message type)
           !is.na(V.4) & !is.na(V.13) & V.13 < 0) #get rid of some messy messages that aren't formatted properly

#rename columns
colnames(ClassA_loc)[1] <- "time"
colnames(ClassA_loc)[2] <- "name"
colnames(ClassA_loc)[3] <- "ship_type"
colnames(ClassA_loc)[8] <- "mmsi"
colnames(ClassA_loc)[13] <- "lon"
colnames(ClassA_loc)[14] <- "lat"
colnames(ClassA_loc)[26] <- "date"
colnames(ClassA_loc)[11] <- "speed"
colnames(ClassA_loc)[16] <- "heading"

#remove extraneous columns
ClassA <- ClassA_loc[,-c(6, 7, 17:29)]

#Save Raw Class A data------------------
#Option 1
#either save this formatted, binded dataframe as a R rds file so we don't have to run this everytime indivdually or bind with previous data
saveRDS(ClassA, file="01_SBARC-AIS_201802-03_ClassA.Rds")
SBARC_AIS_201802_03_ClassA <- readRDS("01_SBARC-AIS_201802-03_ClassA.Rds")

#Option 2
#Bind with previous data and then save as new RDS file
SBARC_AIS_201801_ClassA <- readRDS("01_SBARC-AIS_2018-1_ClassA.Rds")

SBARC_AIS_201801_03_ClassA <- rbind(SBARC_AIS_201801_ClassA, SBARC_AIS_201802_03_ClassA)
saveRDS(SBARC_AIS_201801_03_ClassA, "01_SBARC-AIS_201801-03_ClassA.Rds")

####Add Shipping Lanes--------------------------------
#determine whether Class A points fall within shipping lanes
#load cleaned Class A AIS data
SBARC_AIS_201801_03_ClassA <- readRDS("01_SBARC-AIS_201801-03_ClassA.Rds")

#label all points if they are in the shipping lane or not
#use system time to see how long it takes to calculate
time_start_total <- Sys.time()

#create bbox surrounding shipping lane
ships_channel <- SBARC_AIS_201801_03_ClassA %>%
  filter(lat <= 34.45 &
           lat >= 33.59 &
           lon >= -120.872 &
           lon <= -118.292) # essentially creating a bbox around the shipping lanes

#make a copy of cargo_lane_AIS_points to turn into shapefile
ships_channel_shp <- ships_channel

time_start <- Sys.time()
coordinates(ships_channel_shp) <- ~lon + lat
time_end <- Sys.time()

time_points_to_coordinates <- time_start - time_end

lanes <- readOGR("C:/Users/Caroline/Desktop/Caroline ArcGIS/shipping_lanes","shipping_lanes_combined")

proj4string(ships_channel_shp) <- proj4string(lanes)

#then find points inside of polygons

time_start1 <- Sys.time()
inside.lanes <- !is.na(over(ships_channel_shp, as(lanes, "SpatialPolygons")))
time_end1 <- Sys.time()
time_inside.lanes <- time_start1  - time_end1 

mean(inside.lanes)

#create new column that denotes what lane each point is in (if it is in a lane)
time_start2 <- Sys.time()
ships_channel_shp$lane <- over(ships_channel_shp, lanes)$INFORM
time_end2 <- Sys.time()
time_create_lane_column <- time_start2 - time_end2

#save results as a dataframe
ships_channel_shp@data <- ships_channel_shp@data %>%
  mutate(ship_type=as.integer(ship_type))

ships_channel_2 <- as_data_frame(ships_channel_shp)

time_end_total <- Sys.time()

time_end_total - time_start_total

saveRDS(ships_channel_2, "01_SBARC-AIS_201801-03_ClassA_lanes.Rds")



