

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(sf)
library(tidyverse)

# Directories
indir <- "data/washington/gis_data/raw/"
outdir <- "data/washington/gis_data/processed"
plotdir <- "data/washington/gis_data/figures"


# Read data
coords <- readxl::read_excel(file.path(outdir, "SMA_coordinates.xlsx"))


# Setup
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), scale="large", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot SMAs
  geom_path(data=coords, mapping=aes(x=long_dd, y=lat_dd, group=subunit, color=sma)) +
  geom_point(data=coords, mapping=aes(x=long_dd, y=lat_dd, color=sma)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Special Management Area (SMA)") +
  # Crop
  coord_sf(xlim = c(-125, -123.4), ylim = c(46.8, 48.5)) +
  # Theme
  theme_bw()
g
