
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


sites <- data_orig %>%
  filter(comm_name=="Dungeness crab"  & year <=2016) %>%
  group_by(state, location) %>%
  summarize(n=n(),
            lat_dd=mean(lat_dd),
            long_dd=mean(long_dd))

ggplot(sites, aes(x=long_dd, y=lat_dd, color=state, size=n, label=location)) +
  geom_point() +
  ggrepel::geom_text_repel()

