

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
outdir <- "data/oregon/processed"
plotdir <- "data/oregon/figures"

# Read data
zones <- readxl::read_excel(file.path(outdir, "dcrab_da_mgmt_zones.xlsx"))
zone_lats <- c(zones$lat_dd_north[1], zones$lat_dd_south)

# Read crab sites
crab_sites <- readxl::read_excel(file.path(outdir, "OR_preseason_sampling_sites_dcrab.xlsx"))

# Read site key
# sites_orig <- readxl::read_excel(file.path(indir, "BiotoxSamplingGPS.xlsx")) %>%
#   janitor::clean_names("snake") %>%
#   rename(location=sampling_location, lat_dd=lat, long_dd=long) %>%
#   mutate(species=stringr::str_to_sentence(species))


# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))

# Plot data
g <- ggplot() +
  # Plot management zones
  geom_hline(yintercept=zone_lats) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot sampling sites
  # geom_point(data=sites_orig, mapping=aes(x=long_dd, y=lat_dd, color=species), size=2) +
  geom_point(data=crab_sites, mapping=aes(x=long_dd, y=lat_dd, color=as.character(depth_fathoms)), size=2) +
  # Labels
  labs(x="", y="", title="Oregon biotoxin monitoring sites") +
  scale_color_discrete(name="Depth") +
  # Crop
  coord_sf(xlim = c(-126, -123), ylim = c(41.9, 46.3)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.86, 0.85))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "oregon_da_monitoring_map.png"),
       width=5, height=6.5, units="in", dpi=600)



