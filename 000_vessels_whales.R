#This will identify all ships that are in the thermal camera's field of view

#If you're doing this all in one go (ie using scripts 01 and 02 then you don't need to load the data again and can skip the first line of code)
#load Class A data
raw_SBARC_AIS_data_20180602_ClassA <- readRDS("01_raw_SBARC_AIS_data_20180602_ClassA.Rds")


#creates a rough box to filter the AIS data
test1 <- raw_SBARC_AIS_data_20180602_ClassA %>%
  filter(lon <= -119.73552 & lat >= 34.11976) %>%
  filter(lon >= -119.76814 & lat <= 34.14704)

bbox <- readOGR("C:/Users/Caroline/r_projects/cargo_AIS_tracks/AIS_query.kml", "AIS query.kml")

#just resaving the data as a new df so don't have to reload if mess up (I guess you technically don't need this now, only when creating the code)
test2 <- test1

#load coordinate system
WG84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#make AIS spatial objects
coordinates(test2) <- ~lon + lat

#project AIS data
proj4string(test2) <- WG84_crs

#then find points inside of polygons
test2$test <- over(test2, bbox)$Name

#save results as a dataframe
test2@data <- test2@data %>%
  mutate(ship_type=as.integer(ship_type))

ships_channel_2 <- as_data_frame(test2)

#writes csv if you want it
#write.csv(ships_channel_2, "Ships_field_view_02-06_June.csv", row.names = FALSE)

#this is just a summary table if you want the beginning and end time a vessel was in the bbox - note this may not work for longer time periods b/c I'm just taking in the max and min timestamp, which doesn't account for multiple trips
ships_summary <- ships_channel_2 %>%
  filter(test=="AIS query")%>%
  group_by(mmsi, name, ship_type)%>%
  summarize(min_timestamp=min(datetime_PST), max_timestamp=max(datetime_PST), min_speed=min(speed), max_speed=max(speed))%>%
  mutate(total_time=max_timestamp-min_timestamp)
