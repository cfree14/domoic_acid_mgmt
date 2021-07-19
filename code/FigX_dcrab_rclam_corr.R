
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
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Read management zones
zones_orig <- readxl::read_excel(file.path(outdir, "WC_dcrab_da_mgmt_zones.xlsx")) %>%
  # Remove inland zones
  filter(!is.na(lat_dd_north))

# Extract management zone breaks and labels
zone_labels <- zones_orig$zone_id
zone_breaks <- c( zones_orig$lat_dd_north, zones_orig$lat_dd_south[nrow(zones_orig)])


# Build data
################################################################################

# Goal of analysis
# Calculate and correlate median domoic acid contamination
# by latitude (mgmt zones?) and time bins (2 week intervals?)

# Build data
data_all_spp <- data_orig %>%
  # Categorize spatial zones
  mutate(lat_catg=cut(lat_dd, breaks=zone_breaks, labels = zone_labels)) %>%
  # Categorize temporal zones
  mutate(jweek=week(date),
         jweek_rounded=floor(jweek/2)*2,
         time_catg=paste(year, jweek_rounded, sep="-")) %>%
  # Compute medians
  group_by(comm_name, tissue, lat_catg, time_catg) %>%
  summarize(n=n(),
            da_ppm_med=median(da_ppm)) %>%
  ungroup()

# Extract razor clam
data_rclam <- data_all_spp %>%
  filter(comm_name=="Razor clam" & tissue=="meat") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_rclam=da_ppm_med)

# Extract Dungeness crab
data_dcrab <- data_all_spp %>%
  filter(comm_name=="Dungeness crab" & tissue=="viscera") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_dcrab=da_ppm_med)

# Merge data
data <- data_rclam %>%
  inner_join(data_dcrab, by=c("lat_catg", "time_catg"))

# Plot data
g <- ggplot(data, aes(x=da_ppm_rclam, y=da_ppm_dcrab)) +
  geom_point()
g





