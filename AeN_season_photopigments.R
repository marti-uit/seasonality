#extracting photopigment data from all seasonal cruises.

folder <- "/Users/mar074/Library/CloudStorage/OneDrive-UiTOffice365/PhD UiT/Primary folder/Paper 2 - Seasonality/1- Data and metadata/Photopigments/Pelagic"
files <- list.files(folder, full.names= T)


variables <- c("DEPTH","CHLOROPHYLL_A_TOTAL", "PHAEOPIGMENTS_TOTAL") #vectors of the variables of interest
global_attributes <- c( #attributes of interest
  "cruiseName",
  "stationName",
  "geospatial_lat_min",
  "geospatial_lon_min"
)

headers <- c(global_attributes, variables) #headers of the resulting dataframe
df <- data.frame(matrix(ncol=length(headers),nrow=0)) #defining a dataframe.
colnames(df) <- headers
#checking the variables and attributes.

for (file in files) {
  nc <- open.nc(file)
  print.nc(nc)
}

#funky loop times. we create a dew dataframe for each iteration of the loop, and we append them all together in the end.
for (file in files) {
  nc <- open.nc(file)
  depths <- var.get.nc(nc,"DEPTH") #extracting the depths
  num_depths <- length(depths) #counting how many depths there are in this particular iteration
  newdf <- data.frame(matrix(ncol=length(headers),nrow=num_depths)) #the dataframe will have as many rows as there are depths
  colnames(newdf) <- headers
  
  for (global_attribute in global_attributes) {
   newdf[global_attribute] <- att.get.nc(nc, "NC_GLOBAL", global_attribute)
  }
  
  for (variable in variables) {
    newdf[variable] <- var.get.nc(nc, variable)
  } 
  df <- merge(df,newdf,all=TRUE)
}

#next steps: prepare the dataset for merging with the NPP dataset.
#recoding the variables so that the left_join with other datasets works 
#renaming the variables for the same purpose and making them easier to handle.
#rounding the funky depth values.
#creating a chla/total pigments ratio column.

df <- df %>% 
  mutate(cruiseName = recode(cruiseName,
                             "'2019 Seasonal Study Q3'"= "Q3",
                             "'2019 Seasonal Study Q4'" = "Q4",
                             "'2021 Seasonal Study Q1'" = "Q1",
                             "'2021 Seasonal Study Q2'" = "Q2"),
         stationName = recode(stationName,
                              "P1 (NLEG01)" = "P1",
                              "P2 (NLEG04)" = "P2",
                              "P3 (NLEG07)" = "P3",
                              "P4 (NLEG11)" = "P4",
                              "P5 (NLEG13)" = "P5",
                              "P6 (NLEG21/NPAL15)" = "P6",
                              "P7 (NLEG25/NPAL16)" = "P7",
                              "DEEP-ICE" = "P7"),
         DEPTH = round(DEPTH, digits = 0),
         chla_ratio = CHLOROPHYLL_A_TOTAL/(CHLOROPHYLL_A_TOTAL + PHAEOPIGMENTS_TOTAL)) %>% 
  rename(
         cruise = cruiseName,
         station = stationName,
         lat = geospatial_lat_min,
         long = geospatial_lon_min,
         depth_m = DEPTH,
         chla_ug_L = CHLOROPHYLL_A_TOTAL,
         phaeo_ug_L = PHAEOPIGMENTS_TOTAL )

#amazing! Let's save it for now.
write_csv(df,"1- Data and metadata/Photopigments/AeN_season_photopigments.csv")
