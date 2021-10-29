
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/depth"

# Projections
wgs84 <- "+proj=longlat +datum=WGS84"

# Read bathymetry data
data_orig <- raster::raster("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid/data/bathymetry/raw/ETOPO1_Ice_c_geotiff.tif")
crs(data_orig) <- wgs84

# Format data
data <- data_orig %>%
  # Crop raster
  raster::crop(x=., y=extent(c(-126, -118, 32, 50)) )

# Plot data
plot(data)

# Convert to data frame
data_df <- data %>%
  # Convert to dataframe
  as.data.frame(xy=T) %>%
  # Columns names
  setNames(c("long_dd", "lat_dd", "depth_m")) %>%
  # Depth in feet and fathoms
  mutate(depth_ft=measurements::conv_unit(depth_m, "m", "ft"),
         depth_fathoms=depth_ft/6) %>%
  # Label fishing grounds
  mutate(fishing_yn=depth_fathoms < 0 & depth_fathoms >= -100) %>%
  # Reduce to fishing grounds
  filter(fishing_yn) %>%
  # Reduce to north of Point Conception
  filter(lat_dd >= 34.4486)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Plot data
g <- ggplot() +
  # Fishing grounds
  geom_tile(data=data_df, mapping=aes(x=long_dd, y=lat_dd), fill="grey30") +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(34, 48)) +
  # Theme
  theme_bw()
g






