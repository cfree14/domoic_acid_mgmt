
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
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds")) %>%
  mutate(comm_name=recode(comm_name, "California (sea) mussel"="California mussel"))


# Setup
################################################################################


#
data <- data_orig %>%
  # Species of interest
  filter(comm_name=="California mussel") %>%
  # Calculate stats
  group_by(state, year, month) %>%
  summarize(da_ppm_med=median(da_ppm)) %>%
  ungroup() %>%
  # Standardize
  group_by(state, year) %>%
  mutate(da_ppm_med_scaled=da_ppm_med/max(da_ppm_med)) %>%
  ungroup()

g <- ggplot(data %>% filter(year>=2014), aes(x=month, y=da_ppm_med_scaled, color=year, group=year)) +
  facet_wrap(~state) +
  geom_line() +
  labs(x="Month") +
  theme_bw()
g




