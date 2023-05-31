#extracting CTD data from all seasonal cruises.

folder <- "/Users/mar074/Library/CloudStorage/OneDrive-UiTOffice365/PhD UiT/Primary folder/Paper 2 - Seasonality/1- Data and metadata/CTD/NetCDF"
files <- list.files(folder, full.names= T)


variables <- c("PRES","TEMP","PSAL") #vectors of the variables of interest
global_attributes <- c( #attributes of interest
  "cruiseName",
  "station_name",
  "geospatial_lat_min",
  "geospatial_lon_min",
  "time_coverage_start"
)

headers <- c(global_attributes, variables) #headers of the resulting dataframe
df <- data.frame(matrix(ncol=length(headers),nrow=0)) #defining a dataframe.
colnames(df) <- headers
for (file in files) {
  nc <- open.nc(file)
  print.nc(nc)
}


#funky loop times. we create a dew dataframe for each iteration of the loop, and we append them all together in the end.
for (file in files) {
  nc <- open.nc(file)
  depths <- var.get.nc(nc,"PRES") #extracting the depths
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

df <- df %>% 
  mutate(cruiseName = recode(cruiseName,
                             "2019 Seasonal Study Q3"= "Q3",
                             "2019 Seasonal Study Q4" = "Q4",
                             "2021 Seasonal Study Q1" = "Q1",
                             "2021 Seasonal Study Q2" = "Q2"),
         station_name = recode(station_name,
                              "P1 (NLEG01)" = "P1",
                              "P2 (NLEG04)" = "P2",
                              "P3 (NLEG07)" = "P3",
                              "P4 (NLEG11)" = "P4",
                              "P5 (NLEG13)" = "P5",
                              "P6 (NLEG21/NPAL15)" = "P6",
                              "P7 (NLEG25/NPAL16)" = "P7"),
         PRES = round(PRES, digits = 0)) %>% 
  rename(
    cruise = cruiseName,
    station = station_name,
    lat = geospatial_lat_min,
    long = geospatial_lon_min,
    depth_m = PRES,
    temp_degC = TEMP,
    psal = PSAL )

#amazing, it worked.
#now, converting the CTD data to TEOS-whatever standards, and assigning water masses.
#function gsw_SA_from_SP to convert to absolute salinity from practical salinity

asal <- gsw_SA_from_SP(df$psal, df$depth_m, df$long, df$lat)

#function gsw_CT_from_pt to get conservative temperature from potential temperature.first we calculate potential temperature.

ptemp <- gsw_pt_from_t(asal,df$temp_degC, df$depth, p_ref=0)
ctemp <- gsw_CT_from_pt(asal, ptemp)

#function gsw_pot_rho_t_exact to calculate potential density from absolute salinity,in-situ temperature and sea pressure.

prho <- gsw_pot_rho_t_exact(asal, df$temp_degC, df$depth_m, p_ref=0)

#Adding the TEOS-10 parameters to the dataset, adding a column for water mass and assignations.

df %>% 
  bind_cols(asal, ctemp, prho) %>% 
  dplyr::rename(asal=...8,
                ctemp=...9,
                prho=...10) %>% 
  mutate(
    prho=prho-1000,
    water_mass=
      case_when(
        ctemp <= 0 & prho <= 27.97 ~ "PW",
        ctemp > 0 & asal < 35.06 ~ "wPW",
        ctemp > 2 & asal >= 35.06 ~ "AW",
        ctemp <= 2 & ctemp > 0 & asal >= 35.06 ~ "mAW",
        ctemp <= 0 & ctemp > -1.1 & prho > 27.97  ~ "IW",
        ctemp <= -1.1 & prho > 27.97 ~ "CBSDW",
        ctemp <= 0 & ctemp > -1.1 & asal > 35.06 ~ "EBDW")) -> df

#saving it.
write_csv(df,"1- Data and metadata/CTD/AeN_season_CTD_pstations.csv" )

#overall TS
df %>%
  filter(depth_m > 15,
         station == c("P1","P2","P3","P4","P5","P6","P7","SICE4")) %>% 
  ggplot(aes(asal,ctemp,color=water_mass, shape= cruise)) + 
  geom_point(size=4) + 
  xlab("Absolute salinity (g/kg)") +
  ylab("Conservative temperature (degC)") + 
  scale_color_brewer(palette="Paired") + 
  theme_bw()

#checking for example P6 throughout the seasons.
df %>%
  filter(depth_m > 15,
         station == c("P6")) %>% 
  ggplot(aes(asal,ctemp,color=water_mass, shape= cruise)) + 
  geom_point(size=4) + 
  xlab("Absolute salinity (g/kg)") +
  ylab("Conservative temperature (degC)") + 
  scale_color_brewer(palette="Paired") + 
  theme_bw()
