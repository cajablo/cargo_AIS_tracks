#04 Functions AIS data


###FUNCTION---------------
transit_map <- function(data) {
  #create projection variable
  WG84_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #make sure data is ungrouped by ungrouping it
  data1 <- ungroup(data)
  
  #make mmsis character vector
  data1$mmsi_trip <- as.character(data1$mmsi_trip)
  
  #get unique mmsis, put in id vector
  ids <- unique(data1$mmsi_trip)
  
  #subset data1 so we have the first mmsi_trip number to work with (using id==1) but we are subsetting
  #from the main shape data frame
  first_trip <- data1[data1$mmsi_trip == ids[1],]
  
  #plot lines based on points
  cargo_tanker_map <- list(x=first_trip$lon, y = first_trip$lat)
  cargo_tanker_lines <- map2SpatialLines(cargo_tanker_map, IDs=ids[1])
  
  for(i in 2:length(ids)){
    trip_i <- data1[data1$mmsi_trip == ids[i],]
    cargo_tanker_map <- list(x = trip_i$lon, y = trip_i$lat)
    cargo_tanker_temp <- map2SpatialLines(cargo_tanker_map, IDs = ids[i])
    cargo_tanker_lines <- spRbind(cargo_tanker_lines, cargo_tanker_temp)
  }
  
  #len = sapply(1:length(cargo_tanker_lines), function(i) gLength(cargo_tanker_lines[i,])),
  
  #create a dataframe that we can bind the list of lines to, to create a proper spatial object that has attributes
  #in this case we are creating a dataframe that has the lengths of each line and then renaming the row names to the mmsi trip number
  transit_lengths <- data.frame(mmsi_trip = sapply(1:length(cargo_tanker_lines), function(i) cargo_tanker_lines@lines[[i]]@ID))
  rownames(transit_lengths) <- sapply(1:length(cargo_tanker_lines), function(i) cargo_tanker_lines@lines[[i]]@ID)
  
  #this is used to match to the spatial lines list we create (cargo_tanker_lines) to create a spatial dataframe 
  test_df <- SpatialLinesDataFrame(cargo_tanker_lines, data=transit_lengths)
  
  proj4string(test_df) <- WG84_crs
  
  return(test_df)
}