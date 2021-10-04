
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(raster)
library(tidyverse)
library(rnaturalearth)

# Plotting directory
plotdir <- "figures"

# External data directories
charmdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid/data/charm/processed"
gisdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/gis_data/processed"
landingsdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/dungeness/data/cdfw/landings_public/processed"

# Date to plot
date <- "2015-07-01"
date1 <- gsub("-", ".", paste0("X", date))


# Read data
################################################################################

# Read C-HARM data
pn_brick <- brick(file.path(charmdir, "CHARM_PN_20140305_to_present.grd"))
dap_brick <- brick(file.path(charmdir, "CHARM_DAP_20140305_to_present.grd"))
dac_brick <- brick(file.path(charmdir, "CHARM_DAC_20140305_to_present.grd"))

# Extract one date from C-HARM data
pn_df <- pn_brick[[date1]] %>%
  raster::as.data.frame(xy=T) %>%
  setNames(c("long_dd", "lat_dd", "prob"))
dap_df <- dap_brick[[date1]] %>%
  as.data.frame(xy=T) %>%
  setNames(c("long_dd", "lat_dd", "prob"))
dac_df <- dac_brick[[date1]] %>%
  as.data.frame(xy=T) %>%
  setNames(c("long_dd", "lat_dd", "prob"))


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Setup theme
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  plot.title=element_text(size=8),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=7),
                  legend.position = "bottom",
                  legend.spacing.y = unit(0.05, "cm"),
                  legend.box = "vertical",
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))


# A. Pseudo-nitzschia
#############################

# Plot Pseudo-nitzschia
g1 <- ggplot() +
  # Plot p(Pseudo-nitzschia) raster
  geom_raster(pn_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Raster legend
  scale_fill_gradientn(#name="p(PN ≥10^4 cells/ml)",
                       name=expression("p(PN ≥10"^"4"*" cells/ml)"),
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="A. Pseudo-nitzschia (PN) risk") +
  theme_bw() + my_theme
g1


# B. Particulate domoic acid
#############################

# Plot particulate domoic acid
g2 <- ggplot() +
  # Plot p(pDA) raster
  geom_raster(dap_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Raster legend
  scale_fill_gradientn(name="p(pDA ≥500 ng/l)",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="B. Particulate domoic acid (pDA) risk") +
  theme_bw() + my_theme
g2

# C. Cellular domoic acid
#############################

# Plot cellular domoic acid
g3 <- ggplot() +
  # Plot p(cDA) raster and legend
  geom_raster(dac_df, mapping=aes(x=long_dd, y=lat_dd, fill=prob)) +
  # Add California and Mexico
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Raster legend
  scale_fill_gradientn(name="p(cDA ≥10 pg/cell)",
                       colors=rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
                       limits=c(0,1),
                       na.value=NA) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, ticks.colour = "black", frame.colour = "black"),
         color = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Crop extent
  coord_sf(xlim = c(-125, -116), ylim = c(32, 42)) +
  # Little things
  labs(x="", y="", title="C. Cellular domoic acid (cDA) risk") +
  theme_bw() + my_theme
g3


# Merge plots and export
################################################################################

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "FigS5_charm_maps.png"), width=6.5, height=4, units="in", dpi=600)



