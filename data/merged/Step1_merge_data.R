


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
ordir <- "data/oregon/processed"
outdir <- "data/merged/processed"
plotdir <- "data/merged/figures"

# Read OR data
data_or_orig <- readRDS(file.path(ordir, "ODA_2000_2020_da_sampling_data_final.Rds"))

# Read CA data
data_ca_crab_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid/data/da_sampling/2020_request/processed/CDPH_crab_viscera_da_data.rds")
data_ca_biv_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid/data/da_sampling/2020_request/processed/CDPH_mollusc_viscera_da_data.rds")


# Build data
################################################################################

# Format CA crab data
data_ca_crab <- data_ca_crab_orig %>%
  # Lat/long
  mutate(date=ymd(date),
         lat_dd_use=ifelse(!is.na(lat_dd), lat_dd, block_lat_dd),
         long_dd_use=ifelse(!is.na(long_dd), long_dd, block_long_dd)) %>%
  # Select columns of interest
  select(comm_name, species, date,
         region, area, lat_dd_use, long_dd_use,
         sampleid, da_ppm_prefix, da_ppm) %>%
  # Add columns
  mutate(state="California",
         month=month(date),
         year=year(date),
         type="wild",
         tissue="viscera") %>%
  # Rename columns
  rename(location=area, sci_name=species, lat_dd=lat_dd_use, long_dd=long_dd_use, da_oper=da_ppm_prefix) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything()) %>%
  select(-region)

# Format CA bivalve data
data_ca_biv <- data_ca_biv_orig %>%
  # Add columns
  mutate(date=ymd(date),
         state="California",
         month=month(date)) %>%
  # Rename
  rename(sci_name=species, da_oper=da_ppm_prefix) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything()) %>%
  select(-c(county, type_orig, notes))

# Format OR data
data_or <- data_or_orig %>%
  # Rename
  rename(sampleid=sample_id, tissue=type) %>%
  # Add columns
  mutate(state="Oregon",
         type=ifelse(comm_name=="Dungeness crab", "wild", "unknown")) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything()) %>%
  select(-c(source, type_orig, time))

# Merge data
data <- bind_rows(data_ca_crab, data_ca_biv, data_or)

# Inspect data
str(data)
freeR::complete(data)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))
