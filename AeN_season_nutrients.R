#extracting nutrient data from all seasonal cruises.
#reading in the files

q1 <- read_delim("1- Data and metadata/Nutrients/aen_q1_bottle_data_nutrients_p_stations.csv")
q2 <- read_delim("1- Data and metadata/Nutrients/aen_q2_bottle_data_nutrients_p_stations.csv")
q3 <- read_delim("1- Data and metadata/Nutrients/aen_q3_bottle_data_nutrients.csv")
q4 <- read_delim("1- Data and metadata/Nutrients/aen_q4_bottle_data nutrients 2019711.csv")

#some things to fix, to say the least.
#q1: remove first row and tail rows, modify headers to merge with other datasets.
q1 <- q1 %>% 
  slice(c(-1, -165:-543)) %>% 
  rename(
    depth_m = "pressure",
    NO2_umol_kg = "NO2",
    NO3_umol_kg = "NO3",
    PO4_umol_kg = "PO4",
    Si_umol_kg = "Si",
    lat= "latitude",
    long = "longitude"
  )

#q2: same procedure.
q2 <- q2 %>% 
  slice(c(-1, -164:-521)) %>% 
  rename(
    depth_m = "pressure",
    NO2_umol_kg = "NO2",
    NO3_umol_kg = "NO3",
    PO4_umol_kg = "PO4",
    Si_umol_kg = "Si",
    lat= "latitude",
    long = "longitude"
  )

#q3. mess. different units, not sure if i can equate µmol/L to µmol/kg because i don't know if they are using molality instead of molarity. there is also a contadicting "mL" in the column header. so many NAs in empty columns and rows. 
#so many advantages to sticking to the FAIR principles, why is this in excel and not netCDF? questions.
q3 <- q3 %>% 
  slice(c(-1, -210:-731)) %>% 
  select(-c(21:42)) %>% 
  rename(
    depth_m = "pressure",
    NO2_umol_kg = "no2.ml",
    NO3_umol_kg = "no3.ml",
    PO4_umol_kg = "po4.ml",
    Si_umol_kg = "sioh4.ml",
    lat= "latitude",
    long = "longitude"
  )

#q4. more mess. 
q4 <- q4 %>% 
  slice(c(-1)) %>% 
  select(-19) %>% 
  rename(
    station = "name",
    code = "station",
    depth_m = "pressure",
    NO2_umol_kg = "NO2",
    NO3_umol_kg = "NO3",
    PO4_umol_kg = "PO4",
    Si_umol_kg = "Si(OH)4",
    lat= "latitude",
    long = "longitude"
  )

#I wanted to just append the 4 datasets under each other, but the columns are different in both nature and order, so fuck me i guess.
#i will subset the columns i need and join them in a dataset instead.
q1_use <- q1 %>% 
  select(
    cruise,
    station,
    lat,
    long,
    depth_m,
    NO2_umol_kg,
    NO3_umol_kg,
    PO4_umol_kg,
    Si_umol_kg
  )
q2_use <- q2 %>% 
  select(
    cruise,
    station,
    lat,
    long,
    depth_m,
    NO2_umol_kg,
    NO3_umol_kg,
    PO4_umol_kg,
    Si_umol_kg
  )
q3_use <- q3 %>% 
  select(
    cruise,
    station,
    lat,
    long,
    depth_m,
    NO2_umol_kg,
    NO3_umol_kg,
    PO4_umol_kg,
    Si_umol_kg
  )
q4_use <- q4 %>% 
  select(
    cruise,
    station,
    lat,
    long,
    depth_m,
    NO2_umol_kg,
    NO3_umol_kg,
    PO4_umol_kg,
    Si_umol_kg
  )

#appending the 4 cruises.
nut <- bind_rows(q1_use, q2_use, q3_use, q4_use)
#recoding cruise names, rounding depth to the nearest 10m.
nut <- nut %>% 
  mutate(
    cruise= recode(cruise, 
                   "2021703" = "Q1",
                   "2021704" = "Q2",
                   "2019706" = "Q3",
                   "2019711" = "Q4"),
    depth_m = plyr::round_any(as.numeric(depth_m), 10))
  

#saving.
write_csv(nut,"1- Data and metadata/Nutrients/AeN_season_nutrients.csv")

#ready to merge witht the others!
