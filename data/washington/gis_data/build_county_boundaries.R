

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



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/gis_data/raw/"
outdir <- "data/washington/gis_data/processed"
plotdir <- "data/washington/gis_data/figures"



# Plot data
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")
wa <- usa %>%
  filter(name=="Washington") %>%
  select(geometry)

# WA counties
counties_wa <- tigris::counties(state="Washington", class="sf") %>%
  sf::st_transform(sf::st_crs(wa))

# Crop counties
# plot(counties_wa[5])
counties_wa_clip <- sf::st_intersection(counties_wa, wa)
# plot(wa_clip[5])


# Export data
################################################################################


# Export
saveRDS(counties_wa_clip, file=file.path(outdir, "WA_counties_rnaturalearth_friendly.Rds"))
sf::st_write(counties_wa_clip, dsn=file.path(indir, "WA_counties_rnaturalearth_friendly.shp"))



