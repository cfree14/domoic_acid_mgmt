


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read/ data
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")
sample_sites <- readxl::read_excel("data/tri_state/sampling_sites/2018may_dcrab_da_sampling_sites.xlsx")

# Read proposed sites
sample_sites_ca <- readxl::read_excel("data/california/proposed_expansion/CDFW_2020_proposed_biotoxin_zones_sites.xlsx")

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


# Format data
################################################################################

# Format new CA sample sites
sample_sites_ca_use <- sample_sites_ca %>%
  # Add
  mutate(state="California") %>%
  # Rename
  rename(area=port_area, location=sampling_site) %>%
  # Reduce to new sites
  filter(type=="new") %>%
  mutate(type=recode(type, "new"="Proposed")) %>%
  # Simplify
  select(state, area, location, type, long_dd, lat_dd)

# Merge sample sites
sample_sites_use <- sample_sites %>%
  select(state, area, location, type, long_dd, lat_dd) %>%
  mutate(long_dd=long_dd*-1) %>%
  bind_rows(sample_sites_ca_use) %>%
  mutate(type=recode(type, "required"="Current", "informational"="Current", "optional"="Current"))


# Plot data
################################################################################

# Add average lat
zones <- zones %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2)

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s)

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
g <- ggplot() +
  # Plot management zones
  geom_hline(data=zones, mapping=aes(yintercept=lat_dd_north, color=type), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, color=type, label=zone_id), x=-126.5, hjust=0, size=2, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, hjust=0) +
  # ggrepel::geom_text_repel(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, min.segment.length = 0) +
  # Plot sampling sites
  geom_point(data=sample_sites_use, mapping=aes(x=long_dd, y=lat_dd, shape=type), size=1) +
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, hjust=0) +
  # Plot ports
  # geom_point(data=landings, mapping=aes(x=long_dd, y=lat_dd, size=landings_mt)) +
  # geom_text(data=landings, mapping=aes(x=long_dd, y=lat_dd, label=port_complex), size=2, hjust=0, nudge_x = 0.3) +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_color_manual(name="Zone type", values=c("grey10", "grey60")) +
  scale_shape_manual(name="Site type", values=c(16, 4)) +
  scale_size_continuous(name="Mean seasonal\nlandings (mt)") +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(33, 48)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_wc_dcrab_mgmt_map.png"),
       width=4.5, height=6.5, units="in", dpi=600)
