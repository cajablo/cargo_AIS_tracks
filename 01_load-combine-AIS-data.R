#01_load-combine-AIS-data
##This script loads and combines the hourly AIS data into one big data file that is then saved as a .Rdata file

#path where folder with AIS data is
AIS_file_location <- "./AIS_data_20180602"

# get a list of all of the files in the SBARC data
AIS_files <-
  list.files(path = AIS_file_location,
             pattern = "\\.txt$",
             full.names = TRUE)

# read in txt files 
AIS_files2 <-
  lapply(
    AIS_files,
    read.table,
    sep = ";",
    header = FALSE,
    quote = "",
    na.strings = "NA",
    col.names = paste("V", seq_len(28)),
    fill = TRUE
  )

# for each file add a new column based on the full name of the file (not specificying the full name in the above loop made it not work for some reason)
for (i in 1:length(AIS_files2)) {
  AIS_files2[[i]] <- cbind(AIS_files2[[i]], AIS_files[i])
}

#bind all of the files together
data_rbind <- do.call("rbind", AIS_files2)

#save this formatted, binded dataframe as a R data file so we don't have to run this everytime
saveRDS(data_rbind, file="01_raw_SBARC_AIS_data_20180602-combined.Rds")




