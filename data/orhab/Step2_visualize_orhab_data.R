
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/orhab/data/raw"
outdir <- "data/orhab/data/processed"
plotdir <- "data/orhab/figures"

# Read data
data <- readRDS(file=file.path(outdir, "ORHAB_2000_2020_beach_sampling_data.Rds"))


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position="bottom")


# Temperature
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=temp_c, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="Water temperature (°C)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_temperature.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Salinity
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=salinity, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="Salinity (psu)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_salinity.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Dissolved oxygen
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=do_mg_l, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="Dissolved oxygen (mg/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_dissolved_oxygen_mgL.png"),
       width=6.5, height=7.5, units="in", dpi=600)

# Plot data
g <- ggplot(data, aes(x=week, y=do, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="Dissolved oxygen (%)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_dissolved_oxygen_perc.png"),
       width=6.5, height=7.5, units="in", dpi=600)

# pH
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=ph, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="pH") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_ph.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Chlorophyll a
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=chl_a_ug_l, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Labels
  labs(x="Week of the year", y="Chlorophyll a (ug/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_chlorophyll_a.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Particulate domoic acid
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=pda_ng_l+1, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Scales
  scale_y_continuous(trans="log10") +
  # Labels
  labs(x="Week of the year", y="Particulate domoic acid (ng/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_particulate_domoic_acid.png"),
       width=6.5, height=7.5, units="in", dpi=600)


# Pseudo-nitzschia
#############################

# Plot data
g <- ggplot(data, aes(x=week, y=pn_cells_l+1, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Scales
  scale_y_continuous(trans="log10") +
  # Labels
  labs(x="Week of the year", y="Pseudo-nitzschia density (cells/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_pn_density.png"),
       width=6.5, height=7.5, units="in", dpi=600)

# Plot data
g <- ggplot(data, aes(x=week, y=pn_cells_l_sm+1, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Scales
  scale_y_continuous(trans="log10") +
  # Labels
  labs(x="Week of the year", y="Small Pseudo-nitzschia density (cells/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_pn_density_small.png"),
       width=6.5, height=7.5, units="in", dpi=600)

# Plot data
g <- ggplot(data, aes(x=week, y=pn_cells_l_lg+1, color=year, group=year)) +
  facet_wrap(~site, ncol=5) +
  geom_line() +
  # Scales
  scale_y_continuous(trans="log10") +
  # Labels
  labs(x="Week of the year", y="Large Pseudo-nitzschia density (cells/L)") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Blues")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "ORHAB_pn_density_large.png"),
       width=6.5, height=7.5, units="in", dpi=600)


pda <- data %>%
  filter(!is.na(pda_ng_l))
table(pda$year)
table(pda$site)


orp <- data %>%
  filter(!is.na(orp))
table(orp$year)
table(orp$site)
