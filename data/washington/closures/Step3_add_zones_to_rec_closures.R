

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/raw_data/Files for Chris Free/Rec Closure Logs"
outdir <- "data/washington/closures/data"
plotdir <- "data/washington/closures/figures/temp"
tabledir <- "data/washington/closures/tables"
gisdir <- "data/washington/gis_data/processed"

# Read data
data <- readRDS(file.path(outdir, "WA_DOH_2002_2020_biotoxin_closures_rec.Rds"))

# Read zone key
zone_sf <- readRDS(file.path(gisdir, "WDFW_biotoxin_closure_zones.Rds"))
zone_key <- zone_sf %>%
  sf::st_drop_geometry()
freeR::which_duplicated(zone_key$zone_id)


# Build area key
################################################################################

# Build key
area_key <- data %>%
  # Unique areas
  group_by(action_area) %>%
  summarize(counties=paste(sort(unique(sample_county)), collapse=", ")) %>%
  ungroup() %>%
  # Add zone ids
  left_join(zone_key %>% select(zone_id, zone), by=c("action_area"="zone")) %>%
  mutate(type=ifelse(!is.na(zone_id), "zone", "")) %>%
  # Rename/add
  rename(area_orig=action_area, zone_ids=zone_id) %>%
  mutate(area="") %>%
  # Arrange
  select(area_orig, area, counties, type, zone_ids) %>%
  arrange(counties) %>%
  # Remove
  filter(!is.na(area_orig))

# Export key
write.csv(area_key, file=file.path(outdir, "WA_DOC_rec_closure_area_key.csv"), row.names=F)

