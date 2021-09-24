

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


# Plot data
################################################################################

# Data to plot
data_plot <- data %>%
  filter(nyrs>1)

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
                    # Legend
                    legend.position = c(0.2, 0.12),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
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
ggsave(g, filename=file.path(plotdir, "FigS2_pier_beach_site_map.png"),
       width=2.5, height=4.5, units="in", dpi=600)









