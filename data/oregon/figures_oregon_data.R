
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
data <- readRDS(file.path(outdir, "ODA_2000_2020_da_sampling_data_final.Rds")) %>%
  # Species label
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")"),
         species_label=gsub("\\(NA)", "", species_label))

# Sample sizes
table(data$comm_name)
sum(!data$comm_name %in% c("Dungeness crab", "Razor clam", "California mussel"))

# Time series plots
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=7),
                     plot.title=element_text(size=10),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))

# Annual stats
stats <- data %>%
  # Annual count by species and type
  group_by(comm_name, sci_name, year, type) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  # Species label
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")"),
         species_label=gsub("\\(NA)", "", species_label)) %>%
  # Remove a few species
  filter(!comm_name%in%c("Unknown", "Mussel spp. (tsunami debris)"))

# Sample over time
g1 <- ggplot(stats, aes(x=year, y=n, fill=type)) +
  facet_wrap(~species_label, ncol=4, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples") +
  scale_fill_discrete(name="Sample type") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_or_nsamples_by_year.png"),
       width=6.5, height=5, units="in", dpi=600)


# Contamination boxplots
################################################################################

# Plot boxplot
g1 <- ggplot(data, mapping=aes(x=da_ppm, y=species_label)) +
  geom_boxplot() +
  # Labels
  labs(x="Domoic acid (ppm)", y="") +
  # Scale
  scale_x_continuous(trans="log2", breaks=c(1, 2.5, 5, 10, 25, 50, 100, 250)) +
  # Theme
  theme_bw() + base_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_or_da_boxplots.png"),
       width=6.5, height=4, units="in", dpi=600)


# Spatial-temporal coverage plots
################################################################################

# See regulations here
# https://www.dfw.state.or.us/MRP/shellfish/

# Dungeness crab
################################

# Read data
zones <- readxl::read_excel(file.path(outdir, "dcrab_da_mgmt_zones.xlsx"))
zone_lats <- c(zones$lat_dd_north[1], zones$lat_dd_south)

# Oregon Dungeness crab season
# December 1st to August 14
openers <- paste0(2010:2020, "-12-01") %>% ymd()
closers <- paste0(2011:2021, "-08-14") %>% ymd()
season_key <- tibble(year=paste(2010:2020, 2011:2021, sep="-"),
                     open=openers,
                     close=closers)

# Dungeness crab
g1 <- ggplot(data %>% filter(comm_name=="Dungeness crab"),
             mapping=aes(x=date, y=lat_dd, color=type, size=da_ppm)) +
  # Seasons
  geom_rect(data=season_key, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=47, fill="grey90") +
  # Zones
  geom_hline(yintercept=zone_lats, linetype="dotted") +
  # Samples
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Dungeness crab biotoxin sampling") +
  # Limits
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-01-01"), by="1 year"), labels=2010:2021) +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_discrete(name="Sample type") +
  # Themes
  theme_bw() + base_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_da_samples_dcrab.png"),
       width=6.5, height=3.5, units="in", dpi=600)


# California mussel
################################

# California mussel
g2 <- ggplot(data %>% filter(comm_name=="California mussel"),
             mapping=aes(x=date, y=lat_dd, color=type, size=da_ppm)) +
  # Samples
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="California mussel biotoxin sampling") +
  # Limits
  # scale_x_date(breaks=seq(ymd("2011-01-01"), ymd("2021-01-01"), by="1 year"), labels=2011:2021) +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_discrete(name="Sample type") +
  # Themes
  theme_bw() + base_theme
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigX_da_samples_cmussel.png"),
       width=6.5, height=3.5, units="in", dpi=600)


# Razor clam
################################

# Razor clam
g3 <- ggplot(data %>% filter(comm_name=="Razor clam"),
             mapping=aes(x=date, y=lat_dd, color=type, size=da_ppm)) +
  # Samples
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Razor clam biotoxin sampling") +
  # Limits
  # scale_x_date(breaks=seq(ymd("2011-01-01"), ymd("2021-01-01"), by="1 year"), labels=2011:2021) +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_discrete(name="Sample type") +
  # Themes
  theme_bw() + base_theme
g3

# Export
ggsave(g3, filename=file.path(plotdir, "FigX_da_samples_rclam.png"),
       width=6.5, height=3.5, units="in", dpi=600)

# Other species
################################

# Other species
spp_done <- c("Dungeness crab", "California mussel", "Razor clam")
g4 <- ggplot(data %>% filter(!comm_name %in% spp_done),
            mapping=aes(x=date, y=lat_dd, color=comm_name, size=da_ppm)) +
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Other species biotoxin sampling") +
  # Limits
  scale_y_continuous(limits=c(42,NA)) +
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-01-01"), by="1 year"), labels=2010:2021) +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_discrete(name="Species") +
  # Themes
  theme_bw() + base_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g4

# Export
ggsave(g4, filename=file.path(plotdir, "FigX_da_samples_other.png"),
       width=6.5, height=3.5, units="in", dpi=600)


# Site map
################################################################################

# Build site key
site_key <- data %>%
  mutate(taxa_group=ifelse(comm_name=="Dungeness crab", "Crabs", "Bivalves")) %>%
  group_by(location, lat_dd, long_dd) %>%
  summarize(n=n(),
            types=paste(sort(unique(taxa_group)), collapse="/")) %>%
  ungroup() %>%
  mutate(location)

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
  # geom_hline(yintercept=zone_lats, color="grey60", line_type="dotted") +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot sampling sites
  geom_point(data=site_key, mapping=aes(x=long_dd, y=lat_dd, fill=types, size=n), pch=21) +
  ggrepel::geom_text_repel(data=site_key, mapping=aes(x=long_dd, y=lat_dd, color=types, label=location),
                           size=1.7, max.overlaps = 1000, show.legend = F) +
  # Labels
  labs(x="", y="", title="Oregon biotoxin monitoring sites") +
  # Legends
  scale_size_continuous(name="Number of samples") +
  scale_fill_discrete(name="Type") +
  # Crop
  coord_sf(xlim = c(-127, -121), ylim = c(41.9, 46.3)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5))
#g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_da_sampling_sites.png"),
       width=6.5, height=8, units="in", dpi=300)

