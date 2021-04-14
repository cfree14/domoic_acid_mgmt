

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
harvest_sites_orig <- st_read(file.path(indir, "WDFW_harvest_sites.shp"))
growing_areas_orig <- st_read(file.path(indir, "WDFW_growing_areas.shp"))
closure_zones_orig <- st_read(file.path(indir, "WDFW_closure_zones.shp"))
beaches_orig <- st_read(file.path(indir, "WDFW_beaches.shp"))

# Projections
wgs84 <- "+proj=longlat +datum=WGS84"

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Theme
my_theme <-   theme(axis.text=element_text(size=6),
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


# Format closure zones
################################################################################

# Read key
zone_key <- readxl::read_excel(file.path(outdir, "WDFW_closure_zone_key.xlsx")) %>%
  mutate(zone_id=as.character(zone_id))

# Derive centroids
zone_centroids <- closure_zones_orig %>%
  sf::st_transform(crs=wgs84) %>%
  sf::st_centroid() %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  rename(lat_dd=Y, long_dd=X) %>%
  mutate(zone_id=closure_zones_orig$ZONEID)

# Format
closure_zones <- closure_zones_orig %>%
  # sf::st_drop_geometry() %>%
  # Project
  sf::st_transform(crs=wgs84) %>%
  # Arrange
  select(ZONEID, C_ZNAME, COUNTY) %>%
  # Rename
  rename(zone_id=ZONEID, zone=C_ZNAME, county=COUNTY) %>%
  # Add zone meta data
  mutate(zone_id_lg=sub("\\..*", "", zone_id) %>% as.numeric) %>%
  # Arrange
  select(zone_id_lg, zone_id, zone, everything()) %>%
  # Add key
  left_join(zone_key %>% select(zone_id, basin, area), by="zone_id") %>%
  # Add coordinates
  left_join(zone_centroids, by="zone_id") %>%
  # Arrange
  select(basin, county, area, zone_id_lg, zone_id, zone, lat_dd, long_dd, everything()) %>%
  arrange(basin, county, area, zone_id)

# If working
if(F){
  closure_zones_df <- closure_zones %>%
    sf::st_drop_geometry()
  write.csv(closure_zones_df, file=file.path(outdir, "WDFW_closure_zone_key_raw.csv"), row.names=F)
}

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot closure zones
  # geom_sf(data=closure_zones, mapping=aes(fill=basin), lwd=0.2, color=NA, show.legend = F) +
  geom_sf(data=closure_zones, mapping=aes(fill=basin), lwd=0.2, color="grey30") +
  geom_sf_text(data=closure_zones, mapping=aes(label=zone_id), size=2) +
  # Crop
  coord_sf(xlim=c(-125.1,-121.9), ylim=c(46.2, 49.1)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=8),
        axis.title=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_blank(),
        plot.title=element_text(size=11),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        # Legend
        legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones.pdf"),
       width=8.5, height=11, units="in", dpi=300)

ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones.png"),
       width=8.5, height=11, units="in", dpi=300)

# Export data
saveRDS(closure_zones, file=file.path(outdir, "WDFW_biotoxin_closure_zones.Rds"))


# Create basins key
################################################################################

# Dissolve into basins
basins <- closure_zones %>%
  group_by(basin) %>%
  summarize()
plot(basins)


# Format beaches
################################################################################

# Format beaches
beaches <- beaches_orig %>%
  # Project
  sf::st_transform(crs=wgs84) %>%
  # Simplify
  select(COUNTYNAME, WATERBODYN,
         BEACHID, BIDN, BEACHNAME, ORGANIZATI, GROWINGARE) %>%
  # Rename
  rename(county=COUNTYNAME,
         waterbody=WATERBODYN,
         beach_id1=BEACHID,
         beach_id2=BIDN,
         beach=BEACHNAME,
         organization=ORGANIZATI,
         growing_area=GROWINGARE) %>%
  # Format
  mutate(beach=stringr::str_to_title(beach),
         organization=stringr::str_to_title(organization))

# Insprect
str(beaches)
freeR::complete(beaches)

# Unique ids (neither are unique)
freeR::which_duplicated(beaches$beach_id1)
freeR::which_duplicated(beaches$beach_id2)

# Attributes
table(beaches$county)
table(beaches$waterbody)
table(beaches$beach_id1)
table(beaches$beach_id2)
table(beaches$beach)
table(beaches$organization)
table(beaches$growing_area)

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot beaches
  geom_sf(data=beaches, mapping=aes(color=county), show.legend = F) +
  # Crop
  coord_sf(xlim=c(-125.5,-121.5), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
saveRDS(beaches, file=file.path(outdir, "WDFW_beaches.Rds"))


# Format harvest sites
################################################################################

# Format data
harvest_sites <- harvest_sites_orig %>%
  # Project
  sf::st_transform(crs=wgs84) %>%
  # Rename
  janitor::clean_names("snake") %>%
  select(applicatio, stationnum, growingare, locationde,
         companynam, ownername, contactnam,
         licensenum) %>%
  rename(company=companynam,
         owner=ownername,
         contact=contactnam,
         application_number=applicatio,
         license_number=licensenum,
         station_id=stationnum,
         growing_area=growingare,
         location=locationde) %>%
  # Format
  mutate(company=stringr::str_to_title(company),
         owner=stringr::str_to_title(owner),
         contact=stringr::str_to_title(contact))

# Insprect
str(harvest_sites)
freeR::complete(harvest_sites)

# Unique ids
freeR::which_duplicated(harvest_sites$station_id)
freeR::which_duplicated(harvest_sites$growing_area)
freeR::which_duplicated(harvest_sites$application_number) # unique

# Growing area key
area_key <- harvest_sites %>%
  select(growing_area, station_id) %>%
  sf::st_drop_geometry() %>%
  arrange(growing_area) %>%
  unique()

# Attributes
table(harvest_sites$growing_area)
table(harvest_sites$company)
table(harvest_sites$owner)
table(harvest_sites$contact)

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot harvest sites
  geom_sf(data=harvest_sites, mapping=aes(color=growing_area), show.legend = F) +
  # Crop
  coord_sf(xlim=c(-125.5,-121.5), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export
saveRDS(harvest_sites, file=file.path(outdir, "WDFW_harvest_sites.Rds"))


# Format growing areas
################################################################################

# Notes:
# This could be better as a multipolyogn
# Growing area names/ids are unique but have multiple polygons associated with them

# Step 1. Format growing areas
###############################################

# Format growing areas
growing_areas <- growing_areas_orig %>%
  # sf::st_drop_geometry() %>%
  # Project
  sf::st_transform(crs=wgs84) %>%
  # Rename
  janitor::clean_names("snake") %>%
  select(region, name, growing_are, acres, everything()) %>%
  rename(growing_area=name, growing_area_id=growing_are, area_acres=acres) %>%
  # Recode one growing area id (Gig Harbor and Sinclair Inlet were both 0)
  mutate(growing_area_id=ifelse(growing_area=="Gig Harbor", 9999, growing_area_id)) %>%
  # Arrange
  select(region, growing_area, growing_area_id, area_acres)

# Growing area key
ga_key <- growing_areas %>%
  sf::st_drop_geometry() %>%
  select(region, growing_area, growing_area_id) %>%
  unique()

# Are IDs unique?
freeR::which_duplicated(ga_key$growing_area)
freeR::which_duplicated(ga_key$growing_area_id)

# Step 2. Assign growing areas to basin
###############################################

# Growing areas can fall into multiple basins
# Assisng area to basin in which the majority of area falls

# Assign basin to growing areas
growing_areas_basin <- sf::st_intersection(growing_areas, basins)

# Build final key
growing_areas_basin_key <- growing_areas_basin %>%
  sf::st_drop_geometry() %>%
  group_by(region, growing_area, growing_area_id, basin) %>%
  summarize(n=n(),
            area_acres=sum(area_acres)) %>%
  ungroup() %>%
  group_by(region, growing_area, growing_area_id) %>%
  mutate(total_acres=sum(area_acres)) %>%
  filter(area_acres==max(area_acres)) %>%
  ungroup() %>%
  select(basin, region, growing_area, growing_area_id, total_acres) %>%
  arrange(basin, region, growing_area_id)

# Confirm not duplicated
freeR::which_duplicated(growing_areas_basin_key$growing_area)
freeR::which_duplicated(growing_areas_basin_key$growing_area_id)

# Export
write.csv(growing_areas_basin_key, file=file.path(outdir, "WDFW_commercial_growing_areas_key.csv"), row.names=F)


# Step 3. Add basin to shapefile
###############################################

# Format final growing areas
growing_areas_sf <- growing_areas %>%
  # Add basin
  left_join(growing_areas_basin_key %>% select(growing_area_id, basin), by="growing_area_id") %>%
  # Arrange
  select(basin, region, growing_area, growing_area_id, area_acres) %>%
  arrange(basin, region, growing_area_id)

# Step 4. Plot
###############################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot harvest sites
  geom_sf(data=growing_areas_sf, mapping=aes(color=basin)) +
  # Crop
  coord_sf(xlim=c(-125.5,-121.5), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.1, 0.1))
g

# Export
saveRDS(growing_areas_sf, file=file.path(outdir, "WDFW_growing_areas.Rds"))



