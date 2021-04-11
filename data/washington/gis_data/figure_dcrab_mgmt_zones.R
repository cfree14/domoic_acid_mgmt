

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
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")
sample_sites <- readxl::read_excel("data/tri_state/sampling_sites/2018may_dcrab_da_sampling_sites.xlsx")
sma_coords <- readxl::read_excel(file.path(outdir, "SMA_coordinates.xlsx"))


# Plot data
################################################################################

# Add average lat
zones <- zones %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2)


# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), scale="large", returnclass = "sf")

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
  geom_hline(data=zones, mapping=aes(yintercept=lat_dd_north), linetype="solid", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126, hjust=0, size=3, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot SMAs
  geom_path(data=sma_coords, mapping=aes(x=long_dd, y=lat_dd, group=subunit, color=sma)) +
  # geom_point(data=sma_coords, mapping=aes(x=long_dd, y=lat_dd, color=sma)) +
  geom_text(data=sma_coords %>% filter(corner=="NW") %>% group_by(sma) %>% slice(1),
            mapping=aes(x=long_dd, y=lat_dd, color=sma, label=sma), hjust=1.2, size=3) +
  # Plot sampling sites
  geom_point(data=sample_sites %>% filter(state=="Washington"), mapping=aes(x=long_dd*-1, y=lat_dd), size=2) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-126, -122), ylim = c(46, 49)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_wa_dcrab_mgmt_zones.png"),
       width=4, height=4.5, units="in", dpi=600)




