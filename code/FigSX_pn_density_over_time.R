
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/merged/processed"

# Export data
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_beach_pier_sampling_data_merged.Rds"))


# Build data
################################################################################

# Date labels
date_breaks <- seq("2014-01-01" %>% ymd(), "2021-01-01" %>% ymd(), by="2 weeks")
date_labels <- zoo::rollmean(date_breaks, k=2)

# Build data
data_pda <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pda_ng_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pda_ng_l=max(pda_ng_l, na.rm=T)) %>%
  ungroup() %>%
  # Give order
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))

# Plot data
g <- ggplot(data_pda, aes(x=date_bin, y=site, fill=pda_ng_l+1)) +
  facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Max pDA (ng/L)",
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g

# Build data
data_pn <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pn_tot_cells_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pn_tot_cells_l=max(pn_tot_cells_l, na.rm=T)) %>%
  ungroup() %>%
  # Give order
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))

# Plot data
g <- ggplot(data_pn, aes(x=date_bin, y=site, fill=pn_tot_cells_l+1)) +
  facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Max PN density (cells/L)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom")
g


