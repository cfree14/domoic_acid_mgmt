

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/da_sampling/raw/Files for Chris Free"
outdir <- "data/washington/da_sampling/data"
plotdir <- "data/washington/da_sampling/figures"
gisdir <- "data/washington/gis_data/processed"

# Read data
sites <- read.csv(file.path(outdir, "WA_DOH_biotoxin_sampling_site_key.csv"), as.is=T)

# Read counties
counties_wa <- readRDS(file=file.path(gisdir, "WA_counties_rnaturalearth_friendly.Rds"))


# Plot data
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white", size=3) +
  # Plot sampling sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=county), show.legend = F) +
  # Crop
  coord_sf(xlim=c(-125.5,-121.5), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_wa_da_site_map.png"),
       width=5, height=5.75, units="in", dpi=300)
