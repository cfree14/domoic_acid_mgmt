
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

# Read zones
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx") %>%
  filter(!is.na(lat_dd_north))
zone_lats <- c(zones$lat_dd_north, zones$lat_dd_south[nrow(zones)])


# Build data
################################################################################


# Build data
data <- data_orig %>%
  # Assign Dcrab zone to samples
  mutate(zone_id=cut(lat_dd, breaks=zone_lats, labels=zones$zone_id)) %>%
  # Reduce to razor clam meat samples
  filter(comm_name=="Razor clam" & tissue=="meat") %>%
  # Calculate stats
  group_by(year, month, zone_id) %>%
  summarize(n=n(),
            da_ppm_md=median(da_ppm),
            da_ppm_lo=quantile(da_ppm, probs=0.025),
            da_ppm_hi=quantile(da_ppm, probs=0.975)) %>%
  # Build date
  mutate(date=ymd(paste(year, month, 15, sep="-"))) %>%
  # Add zone meta-data
  left_join(zones %>% select(state, zone_id), by="zone_id") %>%
  # Reduce to years of interest
  filter(year>=2015) %>%
  # Remove zone with little info
  filter(zone_id!="50-K")

# Extract CA Zone B 2016-01 median
zoneB_2016 <- data %>%
  filter(zone_id=="B" & year==2016 & month==1) %>%
  pull(da_ppm)

table(data$type)
table(data$tissue)


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=date, y=da_ppm_md)) +
  facet_wrap(~zone_id, ncol=1, strip.position = "right") +
  # Data
  geom_ribbon(data=data, mapping=aes(x=date, ymin=da_ppm_lo, ymax=da_ppm_hi), fill="grey80", inherit.aes = F) +
  geom_line() +
  # Lines
  geom_hline(yintercept=zoneB_2016) +
  # Limits
  lims(y=c(0, NA)) +
  # Labels
  labs(x="Date", y="Domoic acid contamination (ppm)") +
  # Theme
  theme_bw() + my_theme
g




