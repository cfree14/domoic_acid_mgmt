
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

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_beach_pier_sampling_data_merged.Rds"))

# Read pier sampling sites
sites <- read.csv(file=file.path(datadir, "CA_OR_WA_beach_pier_sampling_sites.csv"), as.is=T) %>%
  # Recode
  mutate(source=recode_factor(source,
                              "ORHAB"="ORHAB",
                              "MERHAB"="ODFW (MOCHA)",
                              "HABMAP"="CA-HABMAP")) %>%
  # More than 1-yr of data
  filter(nyrs > 1)

# Read 100 fathoms
fishing_grounds <- readRDS("data/depth/100fathoms_depth.Rds")

# Build data
################################################################################

# Date labels
max(data_orig$date)
date_breaks <- seq("2014-01-01" %>% ymd(), "2021-10-01" %>% ymd(), by="2 weeks")
date_labels <- zoo::rollmean(date_breaks, k=2)

# Number of bins required
n_bins_req <- 5

# Build data
data_pn <- data_orig %>%
  # Post-2014
  filter(date>="2014-01-01") %>%
  # Add date bin
  mutate(date_bin=cut(date, breaks=date_breaks, labels=date_labels) %>% ymd()) %>%
  # Calculate stats
  filter(!is.na(pn_lg_sm_cells_l)) %>%
  group_by(source, state, site, lat_dd, date_bin) %>%
  summarize(pn_lg_sm_cells_l=max(pn_lg_sm_cells_l, na.rm=T)) %>%
  ungroup() %>%
  # Calculate number of bins available
  group_by(source, state, site, lat_dd) %>%
  mutate(n_bins=n_distinct(date_bin)) %>%
  ungroup() %>%
  # Remove sites with too few bins
  filter(n_bins>=n_bins_req) %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site))) %>%
  filter(!is.na(lat_dd))

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
  # Calculate number of bins available
  group_by(source, state, site, lat_dd) %>%
  mutate(n_bins=n_distinct(date_bin)) %>%
  ungroup() %>%
  # Remove sites with too few bins
  filter(n_bins>=n_bins_req) %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site))) %>%
  filter(!is.na(lat_dd))

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
  # Calculate number of bins available
  group_by(source, state, site, lat_dd) %>%
  mutate(n_bins=n_distinct(date_bin)) %>%
  ungroup() %>%
  # Remove sites with too few bins
  filter(n_bins>=n_bins_req) %>%
  # Arrange
  arrange(lat_dd, date_bin) %>%
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()),
         site=factor(site, levels=unique(site))) %>%
  filter(!is.na(lat_dd))

# Reduce to sites with both types of data
sites_pda <- sort(unique(data_pda$site))
sites_pn_lg <- sort(unique(data_pn_lg$site))
sites_use <- sites_pda[sites_pda%in%sites_pn_lg]

# Subset
data_pda_use <- data_pda %>%
  filter(site %in% sites_use)
data_pn_lg_use <- data_pn_lg %>%
  filter(site %in% sites_use)
sites_use_df <- sites %>%
  filter(site %in% sites_use)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=7),
                   plot.tag = element_text(size=8),
                   plot.tag.position = c(0.007, 0.98),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="right")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot pier sampling sites
g1 <- ggplot() +
  # Plot fishing grounds
  geom_tile(data=fishing_grounds, mapping=aes(x=long_dd, y=lat_dd), fill="grey65") +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.2) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2) +
  # Plot sampling sites
  geom_point(data=sites_use_df, mapping=aes(x=long_dd, lat_dd, fill=source), size=1.5, pch=21, stroke=0.1) +
  # geom_text(data=sites %>% filter(site=="Monterey Wharf"), mapping=aes(x=long_dd, lat_dd, label=site), size=2, hjust=-0.1) +
  # Labels
  labs(x="", y="", title="Beach and pier monitoring", tag="A") +
  scale_y_continuous(breaks=seq(32,48,2)) +
  # Legend
  # scale_fill_manual(name="", values=c("black", "white")) +
  scale_fill_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(32, 48)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        legend.position = c(0.3,0.1),
        legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill=alpha('blue', 0)),
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot data
g2 <- ggplot(data_pn_lg_use, aes(x=date_bin, y=site, fill=pn_lg_cells_l+1)) +
  # facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # State lines
  geom_hline(yintercept=c(7.5, 16.5), lwd=0.2) +
  # Labels
  labs(x="", y="", tag="B", title=expression(italic("Pseudo-nitzschia")*" seriata density")) +
  scale_x_date(date_breaks = "1 year", date_labels="%Y", lim=c(ymd("2014-01-01"), ymd("2021-09-15"))) +
  # Legend
  scale_fill_gradientn(name="Density\n(cells/L)         ",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=10^c(0:6),
                       labels=parse(text=paste0("10^", 0:6))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill=alpha('blue', 0)),
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y=element_text(size=4))
g2

# Plot data
g3 <- ggplot(data_pda_use, aes(x=date_bin, y=site, fill=pda_ng_l+1)) +
  # facet_grid(state~., scale="free_y", space="free_y") +
  geom_tile() +
  # State lines
  geom_hline(yintercept=c(7.5, 16.5), lwd=0.2) +
  # Labels
  labs(x="", y="", tag="C", title="Particulate domoic acid concentration") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y", lim=c(ymd("2014-01-01"), ymd("2021-09-15"))) +
  # Legend
  scale_fill_gradientn(name="Concentration\n(ng/L)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       trans="log10",
                       breaks=10^c(0:4),
                       labels=parse(text=paste0("10^", 0:4))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill=alpha('blue', 0)),
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y=element_text(size=4))
g3

# Merge plots
# g <- gridExtra::grid.arrange(g1, g2, ncol=1)
layout_matrix <- matrix(data=c(1, 2,
                               1, 3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3,
                             layout_matrix=layout_matrix, widths=c(0.3, 0.7))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_midseason_risk_evidence.png"),
       width=6.5, height=3.5, units="in", dpi=600)


# Old stuff
################################################################################

# # Plot data
# g1 <- ggplot(data_pn, aes(x=date_bin, y=site, fill=pn_lg_sm_cells_l+1)) +
#   facet_grid(state~., scale="free_y", space="free_y") +
#   geom_tile() +
#   # Labels
#   labs(x="", y="", tag="A") +
#   scale_x_date(date_breaks = "1 year", date_labels="%Y") +
#   # Legend
#   scale_fill_gradientn(name="Pseudo-nitzschia\ndensity (cells/L)",
#                        colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
#                        trans="log10",
#                        breaks=10^c(0:6),
#                        labels=parse(text=paste0("10^", 0:6))) +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Theme
#   theme_bw() + my_theme
# g1

