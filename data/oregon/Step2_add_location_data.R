
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
workdir <- "data/oregon/intermediate"
outdir <- "data/oregon/processed"
plotdir <- "data/oregon/figures"

# Read data
data1 <- readRDS(file.path(workdir, "ODA_1999_2015_da_sampling_data_clams_mussels.Rds"))
data2 <- readRDS(file.path(workdir, "ODA_2010_2020_da_sampling_data.Rds"))


# Build site key
################################################################################

# Clam/mussel sites
sites1 <- data1 %>%
  group_by(location) %>%
  summarize(species=paste(sort(unique(comm_name)), collapse=", ")) %>%
  ungroup()

# All species sites
sites2 <- data2 %>%
  group_by(location) %>%
  summarize(species=paste(sort(unique(comm_name)), collapse=", ")) %>%
  ungroup()

# Merge sites
sites_orig <- bind_rows(sites1, sites2) %>%
  rename(location_orig=location)

# Export sites
if(F){
  write.csv(sites_orig, file=file.path(workdir, "OR_da_sampling_site_key_incomplete.csv"), row.names=F)
}


# Format data
################################################################################

# Read location key
site_key <- readxl::read_excel(file.path(workdir, "OR_da_sampling_site_key_final.xlsx"))

freeR::which_duplicated(site_key$location_orig)

# Add location info
data1_xy <- data1 %>%
  # Add location information
  rename(location_orig=location) %>%
  left_join(site_key %>% select(location_orig, location, lat_dd, long_dd), by="location_orig") %>%
  # Arrange
  select(comm_name:location_orig, location, lat_dd, long_dd, everything()) %>%
  arrange(comm_name, date, location)

# Inspect
freeR::complete(data1_xy)

# Add location info
data2_xy <- data2 %>%
  # Add location information
  rename(location_orig=location) %>%
  left_join(site_key %>% select(location_orig, location, lat_dd, long_dd), by="location_orig") %>%
  # Arrange
  select(product:location_orig, location, lat_dd, long_dd, everything()) %>%
  arrange(comm_name, date, location, sample_id)

# Inspect
freeR::complete(data2_xy)

# Format data
################################################################################

# Export data
saveRDS(data1_xy, file=file.path(outdir, "ODA_1999_2015_bivalve_data.Rds"))
saveRDS(data2_xy, file=file.path(outdir, "ODA_2010_2020_all_species_data.Rds"))



