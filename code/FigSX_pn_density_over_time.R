
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/merged/processed"
plotdir <- "figures"

# Export data
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_beach_pier_sampling_data_merged.Rds"))

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Build and plot PN data
################################################################################

# Date labels
date_breaks <- seq("2014-01-01" %>% ymd(), "2021-12-31" %>% ymd(), by="2 weeks")
date_labels <- zoo::rollmean(date_breaks, k=2)

# Build data
data_pn <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pn_tot_cells_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pn_tot_cells_l=max(pn_tot_cells_l, na.rm=T)) %>%
  ungroup() %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site)))

# Plot data
g1 <- ggplot(data_pn, aes(x=date_bin, y=site, fill=pn_tot_cells_l+1)) +
  facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Legend
  scale_fill_gradientn(name="Pseudo-nitzschia\ndensity (cells/L)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "FigSX_pn_density_by_site_raster.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Plot data
################################################################################

# Build data
data_pda <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pda_ng_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pda_ng_l=max(pda_ng_l, na.rm=T)) %>%
  ungroup() %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site)))

# Plot data
g2 <- ggplot(data_pda, aes(x=date_bin, y=site, fill=pda_ng_l+1)) +
  facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Legend
  scale_fill_gradientn(name="Particulate domoic acid\nconcentration (ng/L)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=10^c(0:4),
                       labels=parse(text=paste0("10^", 0:4))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "FigSX_pda_density_by_site_raster.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Build and plot PN data
################################################################################

# Build data
data_pn_lg <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pn_lg_cells_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pn_lg_cells_l=max(pn_lg_cells_l, na.rm=T)) %>%
  ungroup() %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site)))

# Plot data
g3 <- ggplot(data_pn_lg, aes(x=date_bin, y=site, fill=pn_lg_cells_l+1)) +
  facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Legend
  scale_fill_gradientn(name="Large Pseudo-nitzschia\ndensity (cells/L)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "FigSX_pn_lg_density_by_site_raster.png"),
       width=6.5, height=6.5, units="in", dpi=600)


