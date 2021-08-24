

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/california/pier_sampling/data/raw"
outdir <- "data/california/pier_sampling/data"

# Where does this data come from?
# CA HABMAP: https://calhabmap.org/
# SCCOOS ERDDAP server home page: https://erddap.sccoos.org/erddap/index.html
# SCCOOS ERDDAP server datasets: https://erddap.sccoos.org/erddap/info/index.html?page=1&itemsPerPage=1000
# Example meta-data

# Piers
# Bodega Bay (not on server), Trindad Pier (barely), Goleta Pier (in all file), Santa Cruz Wharf,
# Monterey Wharf, Cal Poly Pier, Stearns Wharf, Santa Monica Pier, Newport Pier, Scripps Pier


# Format master file (all sites but pre June 1, 2019?)
################################################################################

# Read master data
data1_orig <- read.csv(file.path(indir, "HABs-pre20190601_71e8_1801_a678.csv"), as.is=T)

# Format master data
data1 <- data1_orig %>%
  # Rename columns
  rename("lat_dd"="latitude",
         "long_dd"="longitude",
         "depth_m"="depth",
         "sample_id"="SampleID",
         "location_code"="Location_Code",
         "date_time"="time",
         "temp_c"="Temp",
         "air_temp_c"="Air_Temp",
         "salinity"="Salinity",
         "chl_filtered_ml"= "Chl_Volume_Filtered",
         "chl1_mg_m3"= "Chl1",
         "chl2_mg_m3"= "Chl2",
         "avg_chloro_mg_m3"= "Avg_Chloro",
         "phaeo1_mg_m3"= "Phaeo1",
         "phaeo2_mg_m3"="Phaeo2",
         "avg_phaeo_mg_m3"= "Avg_Phaeo",
         "phosphate_uM"="Phosphate",
         "silicate_uM"="Silicate",
         "nitrite_uM"="Nitrite",
         "nitrite_nitrate_uM"= "Nitrite_Nitrate",
         "ammonium_uM"= "Ammonium",
         "nitrate_uM"="Nitrate",
         "da_vol_filtered_ml"="DA_Volume_Filtered",
         "pda_ng_ml"="pDA",
         "tda_ng_ml"="tDA",
         "dda_ng_ml"="dDA",
         "vol_setttled_for_counting_ml"="Volume_Settled_for_Counting",
         "akashiwo_sanguinea_cells_l"="Akashiwo_sanguinea",
         "alexandrium_spp_cells_l"="Alexandrium_spp",
         "dinophysis_spp_cells_l"="Dinophysis_spp",
         "lingulodinium_polyedra_cells_l"="Lingulodinium_polyedra",
         "prorocentrum_spp_cells_l"="Prorocentrum_spp",
         "pseudo_nitzschia_delicatissima_group_cells_l"="Pseudo_nitzschia_delicatissima_group",
         "pseudo_nitzschia_seriata_group_cells_l"="Pseudo_nitzschia_seriata_group",
         "ceratium_cells_l"="Ceratium",
         "cochlodinium_cells_l"="Cochlodinium",
         "gymnodinium_spp_cells_l"="Gymnodinium_spp",
         "other_diatoms_cells_l"="Other_Diatoms",
         "other_dinoflagellates_cells_l"="Other_Dinoflagellates",
         "total_phytoplankton_cells_l"="Total_Phytoplankton") %>%
  # Remove row of units
  slice(2:nrow(.)) %>%
  # Convert many to numeric
  mutate(across(.cols=c(lat_dd, long_dd, depth_m, temp_c:total_phytoplankton_cells_l), .fns=as.numeric)) %>%
  # Convert many to character
  mutate(across(.cols=sample_id:date_time, .fns=as.character)) %>%
  # Recode location
  mutate(location_code=trimws(location_code),
         location=recode(location_code,
                         "CPP"="CalPoly Pier",
                         "MW"="Monterey Wharf",
                         "SCW"="Santa Cruz Wharf",
                         "NBP"="Newport Pier",
                         "SP"="Scripps Pier",
                         "SMP"="Santa Monica Pier",
                         "SW"="Stearns Wharf",
                         "TP"="Trinidad Pier",
                         "GP"="Goleta Pier")) %>%
  # Fix date
  mutate(date_time=lubridate::ymd_hms(date_time),
         date=lubridate::date(date_time),
         time=format(date_time, format = "%H:%M:%S")) %>%
  # Arrange
  select(location, location_code, lat_dd, long_dd, date_time, date, time, sample_id, everything()) %>%
  # Replace NaNs with NAs
  mutate(across(.cols=sample_id:total_phytoplankton_cells_l, .funs = function(x) ifelse(is.nan(x), NA, x)))

# Inspect
str(data1)
freeR::complete(data1)

# Values
range(data1$date)
sort(unique(data1$location_code))
sort(unique(data1$location))

# Locations
loc_key1 <- data1 %>%
  group_by(location, location_code, lat_dd, long_dd) %>%
  summarize(n=n())

# Export
saveRDS(data1, file=file.path(outdir, "1969_2019_pier_sampling_data.Rds"))


# Format individual files
################################################################################

# Files to read
files2merge <- list.files(indir)
files2merge <- files2merge[files2merge!="HABs-pre20190601_71e8_1801_a678.csv"]

# Merge files
data2_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  fdata_orig <- read.csv(file.path(indir, x))

  # Format data
  fdata <- fdata_orig %>%
    # Rename columns
  rename("lat_dd"="latitude",
         "long_dd"="longitude",
         "depth_m"="depth",
         "sample_id"="SampleID",
         "location_code"="Location_Code",
         "date_time"="time",
         "temp_c"="Temp",
         "air_temp_c"="Air_Temp",
         "salinity"="Salinity",
         "chl_filtered_ml"= "Chl_Volume_Filtered",
         "chl1_mg_m3"= "Chl1",
         "chl2_mg_m3"= "Chl2",
         "avg_chloro_mg_m3"= "Avg_Chloro",
         "phaeo1_mg_m3"= "Phaeo1",
         "phaeo2_mg_m3"="Phaeo2",
         "avg_phaeo_mg_m3"= "Avg_Phaeo",
         "phosphate_uM"="Phosphate",
         "silicate_uM"="Silicate",
         "nitrite_uM"="Nitrite",
         "nitrite_nitrate_uM"= "Nitrite_Nitrate",
         "ammonium_uM"= "Ammonium",
         "nitrate_uM"="Nitrate",
         "da_vol_filtered_ml"="DA_Volume_Filtered",
         "pda_ng_ml"="pDA",
         "tda_ng_ml"="tDA",
         "dda_ng_ml"="dDA",
         "vol_setttled_for_counting_ml"="Volume_Settled_for_Counting",
         "akashiwo_sanguinea_cells_l"="Akashiwo_sanguinea",
         "alexandrium_spp_cells_l"="Alexandrium_spp",
         "dinophysis_spp_cells_l"="Dinophysis_spp",
         "lingulodinium_polyedra_cells_l"="Lingulodinium_polyedra",
         "prorocentrum_spp_cells_l"="Prorocentrum_spp",
          "pseudo_nitzschia_delicatissima_group_cells_l"="Pseudo_nitzschia_delicatissima_group",
          "pseudo_nitzschia_seriata_group_cells_l"="Pseudo_nitzschia_seriata_group",
          "ceratium_cells_l"="Ceratium",
          "cochlodinium_cells_l"="Cochlodinium",
          "gymnodinium_spp_cells_l"="Gymnodinium_spp",
          "other_diatoms_cells_l"="Other_Diatoms",
          "other_dinoflagellates_cells_l"="Other_Dinoflagellates",
          "total_phytoplankton_cells_l"="Total_Phytoplankton") %>%
    # Remove row of units
    slice(2:nrow(.)) %>%
    # Convert many to numeric
    mutate(across(.cols=c(lat_dd, long_dd, depth_m, temp_c:total_phytoplankton_cells_l), .fns=as.numeric)) %>%
    # Convert many to character
    mutate(across(.cols=sample_id:date_time, .fns=as.character))

  # Inspect
  # str(fdata)

})

# Format data
data2 <- data2_orig %>%
  # Recode location
  mutate(location_code=trimws(location_code),
         location=recode(location_code,
                         "CPP"="CalPoly Pier",
                         "HAB_MWII"="Monterey Wharf",
                         "HAB_SCW"="Santa Cruz Wharf",
                         "NP"="Newport Pier",
                         "SIO"="Scripps Pier",
                         "SMP"="Santa Monica Pier",
                         "SW"="Stearns Wharf",
                         "TP"="Trinidad Pier")) %>%
  # Fix date
  mutate(date_time=lubridate::ymd_hms(date_time),
         date=lubridate::date(date_time),
         time=format(date_time, format = "%H:%M:%S")) %>%
  # Fix Trinidad lat/long
  mutate(lat_dd=ifelse(location=="Trinidad Pier", 41.054860, lat_dd),
         long_dd=ifelse(location=="Trinidad Pier", -124.146966, long_dd)) %>%
  # Arrange
  select(location, location_code, lat_dd, long_dd, date_time, date, time, sample_id, everything()) %>%
  # Replace NaNs with NAs
  mutate(across(.cols=sample_id:total_phytoplankton_cells_l, .funs = function(x) ifelse(is.nan(x), NA, x)))

# Inspect
str(data2)
freeR::complete(data2)

# Inspect values
range(data2$date)
sort(unique(data2$location))
sort(unique(data2$location_code))

# Location key
loc_key2 <- data2 %>%
  group_by(location, location_code, lat_dd, long_dd) %>%
  summarize(n=n())

# Export
saveRDS(data2, file=file.path(outdir, "2005_2021_pier_sampling_data.Rds"))

