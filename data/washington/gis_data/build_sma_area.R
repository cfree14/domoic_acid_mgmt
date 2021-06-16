

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
coords <- readxl::read_excel(file.path(outdir, "SMA_coordinates.xlsx"))  %>%
  group_by(subunit) %>%
  mutate(order=1:n()) %>%
  ungroup()


# Build shapefile
################################################################################

# Coordinates to add
coords_add <- matrix(data=c("Makah", "Makah", 5, -124.5, 48.3,
                            "Makah", "Makah", 6, -124.7367, 48.39167, # NE corner
                            "Quileute", "Quileute", 5, -124, 47.96667,
                            "Quileute", "Quileute", 6, -124.6733, 47.96667, # NE corner
                            "Quinault", "Quinault-Split Rock to Raft River", 5, -124, 47.40833,
                            "Quinault", "Quinault-Split Rock to Raft River", 6, -124.3450, 47.46667, # NE corner
                            "Quinault", "Quinault-Copalis River to Split Rock", 5, -124, 47.40833,
                            "Quinault", "Quinault-Copalis River to Split Rock", 6, -124.3333, 47.40833), # NE corner
                     ncol=5, byrow = T, dimnames = list(NULL, c("sma", "subunit", "order", "long_dd", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(long_dd=as.numeric(long_dd),
         lat_dd=as.numeric(lat_dd),
         order=as.numeric(order))

# Expanded coordinates
coords_exp <- bind_rows(coords, coords_add) %>%
  arrange(sma, subunit, order) %>%
  select(sma, subunit, order, long_dd, lat_dd)

coords_list <- list(poly1=coords_exp %>% filter(subunit=="Makah") %>% select(long_dd, lat_dd) %>% as.matrix(),
                    poly2=coords_exp %>% filter(subunit=="Quileute") %>% select(long_dd, lat_dd)%>% as.matrix())

sma_poly <- st_multipolygon(x=coords_list)

# Convert to sf
sma_poly <- sf::st_as_sf(coords_exp, coords=c("long_dd", "lat_dd"), crs="+proj=longlat +datum=WGS84") %>%
  st_cast("LINESTRING", ids=subunit) %>%
  st_cast("POLYGON", ids=subunit)

  group_by(subunit) %>%
  summarise() %>%
  st_cast("POLYGON")

# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), scale="large", returnclass = "sf")

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=sma_poly, mapping=aes(fill=sma)) +
  # Plot SMAs
  geom_path(data=coords_exp, mapping=aes(x=long_dd, y=lat_dd, group=subunit, color=sma)) +
  geom_point(data=coords_exp, mapping=aes(x=long_dd, y=lat_dd, color=sma)) +
  geom_text(data=coords_exp, mapping=aes(x=long_dd, y=lat_dd, label=order), color="black") +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Special Management Area (SMA)") +
  # Crop
  coord_sf(xlim = c(-125, -123.4), ylim = c(46.8, 48.5)) +
  # Theme
  theme_bw()
g

