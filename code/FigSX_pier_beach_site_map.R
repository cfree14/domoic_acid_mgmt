

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/merged/processed"
plotdir <- "figures"

# Read data
data <- read.csv(file=file.path(datadir, "CA_OR_WA_beach_pier_sampling_sites.csv"), as.is=T)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Build zones
################################################################################

# Read zones
zones_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Read SMA polygons
sma_polys <- sf::st_read("data/washington/gis_data/processed/sma_polygons.shp")
# sma_coords <- readxl::read_excel("data/washington/gis_data/processed/SMA_coordinates.xlsx")

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

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
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg)) %>%
  # Reduce to recent
  filter(season=="2020-21 season")

zones_no_ncal_line <- zones %>%
  filter(landmark_north!="Sonoma/Mendocino County Line")

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()



# Plot data
################################################################################

# Data to plot
data_plot <- data %>%
  filter(nyrs>1) %>%
  mutate(source=recode(source, "MERHAB"="ODFW")) %>%
  mutate(source=factor(source, levels=c("HABMAP", "ODFW", "ORHAB") %>% rev()))

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    # Legend
                    legend.position = c(0.2, 0.12),
                    legend.key = element_rect(fill=alpha('blue', 0)),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot SMA
  geom_sf(data=sma_polys, fill="grey60", color=NA) +
  # Plot management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-128, hjust=0, size=1.5, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot Sonoma-Mendocino country line
  geom_hline(yintercept=son_mend_county, linetype="dashed", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot points
  geom_point(data=data_plot, mapping=aes(x=long_dd, y=lat_dd, fill=source), pch=21, size=2) +
  # Label points?
  # ggrepel::geom_text_repel(data=data_plot, mapping=aes(x=long_dd, y=lat_dd, label=site),
  #                          size=2.5) +
  # geom_text(data=data_plot,
  #           mapping=aes(x=long_dd, y=lat_dd, label=site, color=source),
  #           hjust=1.2,  size=2.5, show.legend=F) +
  # Labels
  labs(x="", y="") +
  scale_y_continuous(breaks=seq(32,48,2)) +
  scale_fill_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-128, -117), ylim = c(32, 48)) +
  # Theme
  theme_bw() + base_theme
g


# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_pier_beach_site_map.png"),
       width=2.5, height=4.5, units="in", dpi=600)









