

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/raw_data/Files for Chris Free"
outdir <- "data/washington/da_sampling/data"
plotdir <- "data/washington/da_sampling/figures"
tabledir <- "data/washington/da_sampling/tables"
gisdir <- "data/washington/gis_data/processed"

# Read data
data <- readRDS(file=file.path(outdir, "WA_DOH_2000_2020_biotoxin_sampling_data.Rds"))

# Read counties
counties_wa <- readRDS(file=file.path(gisdir, "WA_counties_rnaturalearth_friendly.Rds"))



# Total sample
################################################################################

# Total sample size
ntot <- data %>%
  group_by(comm_name, sci_name) %>%
  summarize(n_da=sum(!is.na(domoic_ppm)),
            n_psp=sum(!is.na(psp_ug100g)),
            n_dsp=sum(!is.na(psp_ug100g))) %>%
  ungroup() %>%
  mutate(n_tot=n_da+n_dsp+n_psp,
         species_label=paste0(comm_name, " (", sci_name, ")")) %>%
  arrange(desc(n_tot)) %>%
  select(species_label, n_tot, n_da, n_psp, n_dsp)

# Export
write.csv(ntot, file=file.path(tabledir, "TableSX_sample_size.csv"), row.names=F)


# Domoic acid data
################################################################################

# Format domoic acid data
data_da <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")"))

# Sample size over time
################################################################################

# Total sample size
nsamples <-  data_da %>%
  group_by(species_label, comm_name, sci_name) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  arrange(desc(n))

# Themes
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=6),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))

# Tissue type
####################################

# Stats
stats1 <- data_da %>%
  group_by(sample_year, species_label, comm_name, sci_name, domoic_tissue) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g1 <- ggplot(stats1, aes(x=sample_year, y=n, fill=domoic_tissue)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples", title="WA domoic acid sampling by tissue type") +
  # Legend
  scale_fill_discrete(name="Tissue type") +
  # Theme
  theme_bw() + base_theme
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "FigSX_wa_da_nsamples_by_tissue.png"),
       width=6.5, height=4.5, units="in", dpi=300)



# Monitoring type
#####################################

# Stats
stats2 <- data_da %>%
  group_by(sample_year, species_label, comm_name, sci_name, monitoring_type) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g2 <- ggplot(stats2, aes(x=sample_year, y=n, fill=monitoring_type)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples",  title="WA domoic acid sampling by monitoring type") +
  # Legend
  scale_fill_discrete(name="Monitoring type") +
  # Theme
  theme_bw() + base_theme
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "FigSX_wa_da_nsamples_by_monitoring.png"),
       width=6.5, height=4.5, units="in", dpi=300)


# Fresh/frozen
#####################################

# Stats
stats3 <- data_da %>%
  group_by(sample_year, species_label, comm_name, sci_name, fresh_frozen) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g3 <- ggplot(stats3, aes(x=sample_year, y=n, fill=fresh_frozen)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples",  title="WA domoic acid sampling by sample condition") +
  # Legend
  scale_fill_discrete(name="Sample condition") +
  # Theme
  theme_bw() + base_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "FigSX_wa_da_nsamples_by_condition.png"),
       width=6.5, height=4.5, units="in", dpi=300)

# Shell/shucked
#####################################

# Stats
stats4 <- data_da %>%
  group_by(sample_year, species_label, comm_name, sci_name, shell_shucked) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g4 <- ggplot(stats4, aes(x=sample_year, y=n, fill=shell_shucked)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples",  title="WA domoic acid sampling by shell status") +
  # Legend
  scale_fill_discrete(name="Shell status") +
  # Theme
  theme_bw() + base_theme
g4

# Export figure
ggsave(g4, filename=file.path(plotdir, "FigSX_wa_da_nsamples_by_shell_status.png"),
       width=6.5, height=4.5, units="in", dpi=300)


# Domoic acid distribution
################################################################################

# Plot
g <- ggplot(data_da, aes(y=species_label, x=domoic_ppm)) +
  geom_boxplot() +
  # Labels
  labs(x="Domoic acid (ppm)", y="") +
  # Scale
  scale_x_continuous(trans="log2", breaks=c(1, 2.5, 5, 10, 20, 50, 100, 200)) +
  # Theme
  theme_bw() + base_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_wa_da_contam_boxplots.png"),
       width=6.5, height=3, units="in", dpi=300)

# Plot map
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Plot
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=counties_wa, fill="grey80", color="white", lwd=0.3) +
  geom_sf_text(data=counties_wa, mapping=aes(label=NAME), color="white", size=3) +
  # Plot points
  geom_point(data_da, mapping=aes(x=long_dd, y=lat_dd, color=comm_name, size=domoic_ppm)) +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_color_discrete(name="Species") +
  scale_size_continuous(name="Domoic acid (ppm)") +
  # Crop
  coord_sf(xlim=c(-125.5,-122), ylim=c(46, 49.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_wa_da_sample_map.png"),
       width=5.75, height=5.75, units="in", dpi=300)


# Plot time series
################################################################################

# Plot data
g <- ggplot() +
  facet_wrap(~species_label) +
  geom_point(data=data_da, aes(x=sample_date, y=lat_dd, size=domoic_ppm, color=domoic_tissue)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Spatial-temporal coverage of WA domoic acid sampling") +
  scale_color_discrete(name="Tissue type") +
  scale_size_continuous(name="Domoic acid (ppm)") +
  # Theme
  theme_bw() + base_theme
g


# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_wa_da_sample_coverage.png"),
       width=6.5, height=4.5, units="in", dpi=300)




