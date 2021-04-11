


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
data <- readRDS(file=file.path(outdir, "WDFW_growing_areas.Rds"))

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

# Setup
################################################################################

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot growing areas
  geom_sf(data=data, mapping=aes(color=region), fill=NA) +
  # Crop
  coord_sf(xlim=c(-125.5,-121.5), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Region") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.15,0.15),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_wa_comm_growing_areas.png"),
       width=4, height=4.5, units="in", dpi=600)

