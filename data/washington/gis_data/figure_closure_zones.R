

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
data <- readRDS(file=file.path(outdir, "WDFW_biotoxin_closure_zones.Rds")) %>%
  mutate(zone_label=paste0(zone_id, "\n", zone))

# Read counties
counties_wa <- readRDS(file=file.path(outdir, "WA_counties_rnaturalearth_friendly.Rds"))

# Projections
wgs84 <- "+proj=longlat +datum=WGS84"

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")


# Plot full map
################################################################################

# County-zone key
county_key <- data %>%
  sf::st_drop_geometry() %>%
  group_by(county) %>%
  summarize(zone_ids=paste(sort(unique(zone_id)), collapse=", "))

# Export
write.csv(county_key, file=file.path(outdir, "WA_county_zone_key.csv"), row.names=F)

# Plot full map
################################################################################

# Theme
my_theme <-   theme(axis.text=element_text(size=8),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_blank(),
                    plot.title=element_text(size=11),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=basin), lwd=0.2, color="grey30") +
  # geom_sf_text(data=data, mapping=aes(label=zone_id), size=2) +
  ggrepel::geom_text_repel(data=data, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, max.overlaps = 9999, segment.size=0.3) +
  # Crop
  coord_sf(xlim=c(-125.1,-121.9), ylim=c(46.2, 49.1)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones.pdf"),
       width=8.5, height=11, units="in", dpi=300)

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones.png"),
       width=8.5, height=11, units="in", dpi=300)

# Pacific / SJF
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data %>% filter(basin %in% c("Pacific Coast", "Strait of Juan de Fuca")), mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend = F) +
  geom_sf_text(data=data %>% filter(basin %in% c("Pacific Coast", "Strait of Juan de Fuca")), mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-124.9,-122.7), ylim=c(46.4, 48.5)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_pacific.pdf"),
       width=8.5, height=11, units="in", dpi=300)


# Puget Sound
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend=F) +
  geom_sf_text(data=data, mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-123.2,-122.25), ylim=c(47.05, 47.9)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_puget.pdf"),
       width=8.5, height=11, units="in", dpi=300)


# South Basin
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend=F) +
  geom_sf_text(data=data, mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-123.2,-122.25), ylim=c(47.05, 47.4)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_puget_south.pdf"),
       width=11, height=8.5, units="in", dpi=300)

# Willipa Bay
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend=F) +
  geom_sf_text(data=data, mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-124.1,-123.65), ylim=c(46.35, 46.75)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_willipa.pdf"),
       width=8.5, height=11, units="in", dpi=300)


# San Juan Islands
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend=F) +
  geom_sf_text(data=data, mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-123.3,-122.4), ylim=c(48.1, 49)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_san_juan.pdf"),
       width=8.5, height=11, units="in", dpi=300)

# Whidbey
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white") +
  # Plot closure zones
  geom_sf(data=data, mapping=aes(fill=county), lwd=0.2, color="grey30", show.legend=F) +
  geom_sf_text(data=data, mapping=aes(label=zone_label), size=2) +
  # Crop
  coord_sf(xlim=c(-123,-122.1), ylim=c(47.8, 48.5)) +
  # Labels
  labs(x="", y="", title="Washington's biotoxin closure zones") +
  # Legend
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.16, 0.13),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plots
ggsave(g, filename=file.path(plotdir, "WDFW_closure_zones_whidbey.pdf"),
       width=8.5, height=11, units="in", dpi=300)

