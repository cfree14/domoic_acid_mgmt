


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read zones
zones_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Read tri-state sampling sites
sample_sites2014_orig <- readxl::read_excel("data/tri_state/sampling_sites/2014jul_dcrab_da_sampling_sites.xlsx")
sample_sites2020_orig <- readxl::read_excel("data/tri_state/sampling_sites/2020may_dcrab_da_sampling_sites.xlsx")

# Read California sampling sites
sample_sites_ca <- readxl::read_excel("data/california/zones/CDFW_2020_proposed_biotoxin_zones_sites.xlsx")

# Read SMA coordinates
sma_coords <- readxl::read_excel("data/washington/gis_data/processed/SMA_coordinates.xlsx")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build port data
################################################################################

# Read PACFIN data
data_pacfin_orig <- wcfish::pacfin_crab2
ports <- wcfish::pacfin_ports

# Tillamook: 45.455278, -123.8425
# North Puget Sound: 47.851654, -122.426350
# South Puget Sound: 47.310069, -122.473322
# Washington Coastal: 46.800477, -124.090642
# Columbia River (Washington): 46.290561, -123.702335
# Columbia River (Oregon): 46.190926, -123.690523
latlongs_missing <- matrix(data=c("Tillamook", 45.455278, -123.8425,
                                  "North Puget Sound", 47.851654, -122.426350,
                                  "South Puget Sound", 47.310069, -122.473322,
                                  "Washington Coastal", 46.800477, -124.090642,
                                  "Columbia River (Washington)", 46.290561, -123.702335,
                                  "Columbia River (Oregon)", 46.190926, -123.690523),
                           ncol=3, byrow=T, dimnames = list(NULL, c("port_complex", "lat_dd2", "long_dd2"))) %>%
  as.data.frame() %>%
  mutate(lat_dd2=as.numeric(lat_dd2),
         long_dd2=as.numeric(long_dd2))

# Build data
landings <- data_pacfin_orig %>%
  # Calculate season totals
  group_by(state, port_complex, season) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Reduce to recent seasons
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>%
  filter(year>=2010 & year<2020) %>%
  # Calculate recent average
  group_by(state, port_complex) %>%
  summarize(nseasons=sum(landings_mt>0),
            landings_mt=mean(landings_mt)) %>%
  ungroup() %>%
  # Remove ports
  filter(port_complex!="Withheld For Confidentiality**" & !grepl("Other Or Unknown", port_complex) & nseasons>0) %>%
  # Format port names
  mutate(port_complex=gsub(" Area Ports| Ports", "", port_complex)) %>%
  # Add lat/long
  left_join(ports %>% select(port_name, lat_dd, long_dd), by=c("port_complex"="port_name")) %>%
  # Add missing lat/longs
  left_join(latlongs_missing, by="port_complex") %>%
  mutate(lat_dd=ifelse(is.na(lat_dd), lat_dd2, lat_dd),
         long_dd=ifelse(is.na(long_dd), long_dd2, long_dd)) %>%
  # Reduce
  select(-c(lat_dd2, long_dd2))


# Build zones and sites
################################################################################

# Build zones
##########################################

# Build zones
zones1 <- zones_orig %>%
  filter(state=="Washington") %>%
  mutate(season="2015-16 season") %>%
  select(season, everything())
zones2 <- zones_orig %>%
  mutate(season="2020-21 season") %>%
  select(season, everything())
zones <- bind_rows(zones1, zones2) %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()


# Build sampling sites
##########################################

# Format new CA sample sites
sample_sites_ca_use <- sample_sites_ca %>%
  # Add
  mutate(state="California") %>%
  # Rename
  rename(area=port_area, location=sampling_site) %>%
  # Reduce to new sites
  filter(type=="new") %>%
  # Simplify
  select(state, area, location, long_dd, lat_dd)

# Merge 2020 tri-state and 2020 CA sites
sample_sites2020 <- sample_sites2020_orig %>%
  select(state, area, location, long_dd, lat_dd) %>%
  mutate(long_dd=long_dd*-1) %>%
  bind_rows(sample_sites_ca_use) %>%
  mutate(season="2020-21 season")

# Build
sample_sites <- sample_sites2014_orig %>%
  mutate(season="2015-16 season") %>%
  select(season, state, area, location, long_dd, lat_dd) %>%
  mutate(long_dd=long_dd*-1) %>%
  bind_rows(sample_sites2020)


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(zones) +
  # Facet by season
  facet_wrap(~season) +
  # Plot management zones
  geom_hline(data=zones, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126.5, hjust=0, size=2, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot SMAs
  geom_path(data=sma_coords, mapping=aes(x=long_dd, y=lat_dd, group=subunit)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot ports
  # geom_point(data=landings, mapping=aes(x=long_dd, y=lat_dd, size=landings_mt), color="grey50", pch=21) +
  # # geom_text(data=landings, mapping=aes(x=long_dd, y=lat_dd, label=port_complex), color="grey50", size=2, hjust=0, nudge_x = 0.4) +
  # ggrepel::geom_text_repel(data=landings, mapping=aes(x=long_dd, y=lat_dd, label=port_complex), color="grey50", size=2, hjust=0, nudge_x = 0.5, segment.color=NA) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, hjust=0) +
  # Plot sampling sites
  geom_point(data=sample_sites, mapping=aes(x=long_dd, y=lat_dd), size=1, color="darkred") +
  # geom_text(data=sample_sites, mapping=aes(x=long_dd, y=lat_dd, label=location), size=1, color="darkred") +
  # Labels
  labs(x="", y="") +
  # Legends
  # scale_size_continuous(name="Mean seasonal\nlandings (mt)", range=c(0.1,6)) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(35, 48)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.1, 0.15))
g


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_wc_dcrab_mgmt_map.png"),
       width=6.5, height=5.75, units="in", dpi=600)




