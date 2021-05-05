


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir_or <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/odfw/confidential/processed/"
datadir_ca <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/landings_confidential/processed/"

# Read PACFIN data
data_pacfin_orig <- wcfish::pacfin_crab2
ports <- wcfish::pacfin_ports


# Format data
################################################################################

# Tillamook: 45.455278, -123.8425
# North Puget Sound: 47.851654, -122.426350
# South Puget Sound: 47.310069, -122.473322
# Washington Coastal: 46.800477, -124.090642
# Columbia River (Washington): 46.290561, -123.702335
# Columbia River (Oregon): 46.190926, -123.690523
latlongs_missing <- matrix(data=c("Tillamook", 45.455278, -123.8425,
                                  "North Puget Sound", 47.851654, -122.426350,
                                  "South Puget Sound", 47.310069, -122.473322,
                                  "Washington Coastal", 46.800477, -124.090642,
                                  "Columbia River (Washington)", 46.290561, -123.702335,
                                  "Columbia River (Oregon)", 46.190926, -123.690523), ncol=3, byrow=T, dimnames = list(NULL, c("port_complex", "lat_dd2", "long_dd2"))) %>%
  as.data.frame()

# Build data
data <- data_pacfin_orig %>%
  # Calculate season totals
  group_by(state, port_complex, season) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Reduce to recent seasons
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>%
  filter(year>=2010 & year<2020) %>%
  # Calculate recent average
  group_by(state, port_complex) %>%
  summarize(nseasons=sum(landings_mt>0),
            landings_mt=mean(landings_mt)) %>%
  ungroup() %>%
  # Remove ports
  filter(port_complex!="Withheld For Confidentiality**" & !grepl("Other Or Unknown", port_complex) & nseasons>0) %>%
  # Format port names
  mutate(port_complex=gsub(" Area Ports| Ports", "", port_complex)) %>%
  # Add lat/long
  left_join(ports %>% select(port_name, lat_dd, long_dd), by=c("port_complex"="port_name")) %>%
  # Add missing lat/longs
  left_join(latlongs_missing, by="port_complex") %>%
  mutate(lat_dd=ifelse(is.na(lat_dd), lat_dd2, lat_dd),
         long_dd=ifelse(is.na(long_dd), long_dd2, long_dd)) %>%
  # Reduce
  select(-c(lat_dd2, long_dd2))









