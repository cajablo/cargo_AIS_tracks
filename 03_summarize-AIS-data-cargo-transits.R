#03_summarize-AIS-data-cargo-transits

#This R script aims to organzie and summarize AIS data cargo transits in the SB channel, including identifying cargo transits, summarizing patterns
#and visualizing such data

#load libraries
library(pacman) #I heard about this package at an R meeting - essentially if you load this package and then do p_load for the rest of the libraries then if it's not installed it will automatically install it for you, otherwise it just loads it
p_load(dplyr)
p_load(ggplot2)
p_load(sp)
p_load(rgdal)
p_load(maps)
p_load(maptools)
p_load(rgeos)

#load function script
source("04_Functions-AIS-data.R")

#load cleaned Class A AIS data
ships_channel_2 <- readRDS("01_SBARC-AIS_201801-03_ClassA_lanes.Rds")

#filter so only look at defined cargo ships and tankers for now (Ship type 70-89)------------------
cargo_tankers_channel <- ships_channel_2 %>%
  filter(ship_type >= 70 & ship_type < 90)

# Get mmsis that have travelled through the northern and southernd ends of the shipping lanes
# ships in bbox of northern beginning part of shipping lane
mmsi_lanes_north <- cargo_tankers_channel %>%
  filter((lat <= 34.47 & lat >= 34.36 & lon >= -120.9 & lon <= 120.8)) %>%
  group_by(mmsi, name)%>%
  summarize(AIS_points_n = n())
# ships in bbox of ships_channel_2 beginning part of shipping lane
mmsi_lanes_south <- cargo_tankers_channel %>%
  filter((lat <= 33.68 & lat >= 33.576 & lon >= -118.326 & lon <= 118.265)) %>%
  group_by(mmsi, name)%>%
  summarize(AIS_points_s = n())

#create df of just mmsis that were in the bbox of the beginning/end of cargo lanes
join_lanes <- mmsi_lanes_south %>%
  full_join(mmsi_lanes_north, by="mmsi")%>% #use a full join to get all of the ships that went through both bbox
  na.omit()%>%
  select(mmsi)

#get just AIS points for mmsis that were in both bboxes of the cargo lane, and then create transit numbers for each transit
cargo_tankers_lane_transits <- cargo_tankers_channel %>%
  right_join(join_lanes, by = "mmsi") %>%
  group_by(mmsi) %>%
  arrange(datetime_PST)%>%
  mutate(mmsi_seq = row_number(), time_diff_hrs = (c(0, diff(datetime)) /   #have to start the first point for each ship with 0 (can't use NA otherwise creating new transits doesn't work)
                                                     (3600))) %>%
  mutate(trip = cumsum(time_diff_hrs > 12) + 1L) %>% #this creates a new trip/transit for all ships, it starts once the time time difference between two points is > 12 hours
  mutate(mmsi_trip = paste0(mmsi, "-", trip)) %>% #creates a unique transit identifier based on the mmsi and trip number, essentially MMSI-TRIP
  mutate(heading_diff=(c(0,diff(heading))))

#create a dataframe with mmsi trips we don't want (starting from scratch you'd have to create the summary table first, plot the lines and figure out which ones you don't want and then come back here and remove them) - ideally I will figure out a way so that we don't have to do this based but I'm obviously not there yet
mmsi_trip_remove <- c("211327410-1", "211327410-3", "356872000-5", "355717000-1", "636014557-6", "352776000-2")

mmsi_trip_remove_df <- data.frame(mmsi_trip_remove, stringsAsFactors=FALSE)

#create summary table for each trip
cargo_tankers_lane_transits_summary <- cargo_tankers_lane_transits %>%
  ungroup()%>%
  filter(speed < 30)%>%
  anti_join(mmsi_trip_remove_df, by=c("mmsi_trip"="mmsi_trip_remove"))%>%
  #filter((heading < 140 & heading > 75) | (heading > 250 & heading < 320))%>%
  group_by(mmsi, name, ship_type, mmsi_trip) %>%
  #get max/mins for various columns
  summarize(
    min_time = min(datetime_PST),
    max_time = max(datetime_PST),
    median_time = median(datetime_PST),
    avg_speed = mean(speed),
    max_speed = max(speed),
    min_speed = min(speed),
    AIS_points = n(),
    AIS_points_lane = sum(!is.na(lane)),
    max_lon = max(lon),
    min_lon = min(lon),
    max_lat = max(lat),
    min_lat = min(lat),
    max_heading =max(heading), 
    min_heading= min(heading),
    max_heading_diff=max(heading_diff), 
    heading_diff_50=length(mmsi_seq[heading_diff>50]),
    heading_diff_100=length(mmsi_seq[heading_diff>100])) %>%
  #calculate the length of the transit (time_diff) as well as the differences between the max and min lat/lon (respectively).
  mutate(
    time_diff = as.numeric(difftime(max_time, min_time, units="hours")),
    lon_diff = max_lon - min_lon,
    lat_diff = max_lat - min_lat,
    prop_AIS_in_lane = AIS_points_lane/AIS_points
  )  %>%
  #essentially we are making sure that the transits are the full length (2.4 decimal degrees in lon and 0.7 degrees in lat) of the the shipping lane (although this doesn't always seem to be the case...)
  filter(lon_diff > 2.4 & lat_diff > .7 & prop_AIS_in_lane > 0.95)

saveRDS(cargo_tankers_lane_transits_summary, file="cargo_tankers_lane_transits_summary.RDS")

#get these mmsis from cargo tanker transits, and filter AIS point to only these tranists (from the summary table)
cargo_tanker_refined_mmsi <- unique(cargo_tankers_lane_transits_summary$mmsi_trip)

cargo_tanker_lane_transits_refined <- subset(cargo_tankers_lane_transits, mmsi_trip %in% cargo_tanker_refined_mmsi)
cargo_tanker_lane_transits_refined_30 <- subset(cargo_tanker_lane_transits_refined, speed <= 30)

#create polylines based on individual transits
#make sure no NAs in dataframe
colSums(is.na(cargo_tanker_lane_transits_refined_30))


#use function (from Function script) to create polylines of each transit
test <- transit_map(data = cargo_tanker_lane_transits_refined_30)

#plot the polylines
plot(test)

cargo_tanker_transit_lines <- sp::merge(test, cargo_tankers_lane_transits_summary, by.x = "mmsi_trip", by.y="mmsi_trip")

writeOGR(cargo_tanker_transit_lines, dsn="." ,layer="cargo_tanker_transit_lines_test2",driver="ESRI Shapefile")

#March Summary Transit Table ---------------------
#From 03-summary AIS data
Mar_cargo_transits <- cargo_tankers_lane_transits_summary %>%
  filter(max_time >="2018-03-01 -00:00:00 PST"  & prop_AIS_in_lane > .9)

#explore cargo transits errant points----
#this looks at individual trips to see where the points are in that trip - I've been saving it and then exploring the file in QGIS to get a better sense of what's going on with some of the weird errant points in a vessel's trip
Resolute_bay_1 <- cargo_tanker_lane_transits_refined %>%
  filter(mmsi_trip=="232005179-1")
coordinates(Resolute_bay_1) <- ~lon + lat

WG84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(Resolute_bay_1) <- WG84_crs

writeOGR(Resolute_bay_1, dsn="." ,layer="Resolute_bay_1",driver="ESRI Shapefile")


