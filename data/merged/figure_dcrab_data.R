


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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Read OR zones
zones_or <- readxl::read_excel(file.path(ordir, "dcrab_da_mgmt_zones.xlsx"))
zone_lats_or <- c(zones_or$lat_dd_north[1], zones_or$lat_dd_south)

# Setup
################################################################################

# Data
data <- data_orig %>%
  filter(comm_name=="Dungeness crab" & date >= ymd("2000-01-01") &
           !is.na(date) & !is.na(lat_dd) & !is.na(da_ppm))

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
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
g <- ggplot(data, aes(x=date, y=lat_dd, size=da_ppm, color=da_ppm)) +
  # Season shading
  # Sampling points
  geom_point(alpha=0.8) +
  # State/region lines
  geom_hline(yintercept=c(48.48, 41.9981226, 46.15), size=0.5) +
  geom_hline(yintercept = 38.143562, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Management zone lines
  geom_hline(yintercept = zone_lats_or, linetype="dotted", size=0.5, color="grey30") +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-01-01"), by="1 year"), labels=2010:2021) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_gradientn(name="Domoic acid (ppm)", colors=RColorBrewer::brewer.pal(9, "Reds")[4:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_dcrab_da.png"),
       width=6.5, height=4, units="in", dpi=600)
