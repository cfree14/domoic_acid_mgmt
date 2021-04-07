


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/merged/processed"

# Read DA sampling data
samples_orig <- readRDS(file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))

# Read landings data
dcrab_orig <- wcfish::pacfin_crab2
pacfin_ports <- wcfish::pacfin_ports


# Landings per season
################################################################################

# Calculate samples per season
samples <- dcrab_orig %>%
  # Reduce to Dungeness crab
  filter(comm_name=="Dungeness crab" & date>=ymd("2000-01-01")) %>%
  # Add season marker
  mutate(season=ifelse(month>=10, paste(year, year+1, sep="-"), paste(year-1, year, sep="-"))) %>%
  # Summarize by state and season
  group_by(state, season) %>%
  summarize(nsamples=n()) %>%
  ungroup()

# Calculate landings per season
dcrab <- dcrab_orig %>%
  # Summarize by state and season
  group_by(season, state, port_code, port_complex) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Calculate 10 year mean
  mutate(year1=substr(season,1,4) %>% as.numeric) %>%
  filter(year1>=2010) %>%
  group_by(state, port_code, port_complex) %>%
  summarize(landings_mt=mean(landings_mt)) %>%
  ungroup() %>%
  # Add lat/long
  left_join(wcfish::pacfin_ports %>% select(port_code, lat_dd, long_dd, port_yn))

# Build dataset
data <- samples %>%
  left_join(landings) %>%
  mutate(samples_per_mt=nsamples/landings_mt,
         mt_per_sample=landings_mt/nsamples) %>%
  # Remove recent season
  filter(season!="2020-2021")

# Plot
g <- ggplot(data, mapping=aes(x=season, y=mt_per_sample, fill=state)) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Season", y="Landed crab (mt) per sampled crab") +
  theme_bw()
g
