


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
or_zones <- readxl::read_excel(file.path("data/oregon/dcrab_da_mgmt_zones.xlsx"))
ca_zones <- readxl::read_excel("data/california/proposed_expansion/CDFW_2020_proposed_biotoxin_zones_sites.xlsx", sheet = 2)
# wa_zones <- readxl::read_excel(file.path("data/washington/dcrab_da_mgmt_zones_wa.xlsx"))
wa_zones <- sf::st_read(dsn="data/washington/gis_data", layer="WDFW_closure_zones") %>%
  sf::st_transform("+proj=longlat +datum=WGS84")
sample_sites <- readxl::read_excel(file.path("data/tri_state/sampling_sites/2018may_dcrab_da_sampling_sites.xlsx"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

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
g <- ggplot() +
  # Plot management zones
  geom_sf(data=wa_zones, fill=NA, color="grey30", lwd=0.1) +
  # geom_hline(data=wa_zones, mapping=aes(yintercept=lat_dd)) +
  geom_hline(data=or_zones, mapping=aes(yintercept=lat_dd_south)) +
  geom_hline(data=ca_zones, mapping=aes(yintercept=lat_dd), linetype="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot sampling sites
  geom_point(data=sample_sites, mapping=aes(x=long_dd*-1, y=lat_dd, color=state)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(33, 48)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.2, 0.15))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "da_sampling_mgmt_zone_map.png"),
       width=3.5, height=6.5, units="in", dpi=600)
