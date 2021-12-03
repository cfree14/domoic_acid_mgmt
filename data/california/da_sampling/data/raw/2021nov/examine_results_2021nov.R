


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/merged/processed"
workdir <- "data/california/da_sampling/data/raw/2021nov/"

# Read data
data_orig <- read.csv("data/california/da_sampling/data/raw/2021nov/CrabDAWebResultsJuly12021toNovember242021.csv", as.is=T)

# Read zones
zones_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Read sites
sites_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_sampling_sites.xlsx", sheet=2)

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60



# Build zones
################################################################################

# Build zones
zones <- zones_orig %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))

zones_no_ncal_line <- zones %>%
  filter(landmark_north!="Sonoma/Mendocino County Line")

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()


# Build data
################################################################################

# Format sites
sites <- sites_orig %>%
  rename(site=location) %>%
  filter(state=="California") %>%
  select(site, lat_dd, long_dd)

# Build data
data <- data_orig %>%
  # Format sites
  mutate(site=recode(site,
                     "George Reef"="St. George Reef",
                     "Pillar Point"="Pillar Point (Half Moon Bay)",
                     "Avila Beach"="Avila")) %>%
  # Add lat/long
  left_join(sites, by="site") %>%
  # Arrange
  select(area, port, site, lat_dd, long_dd, everything()) %>%
  # Format results
  mutate(da_ppm_avg=recode(da_ppm_avg, "Non-Detect"="0") %>% as.numeric(),
         percent_over=gsub("%", "", percent_over) %>% as.numeric(),
         status=ifelse(percent_over>1/6*100, "Contaminated\n(>1 in 6 above action level)", "Clean\n(0 in 6 above action level)"),
         status=factor(status, levels=c("Clean\n(0 in 6 above action level)", "Contaminated\n(>1 in 6 above action level)")))



# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5.5),
                    legend.title=element_text(size=6.5),
                    plot.title=element_blank(),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot site map
g <- ggplot(zones) +
  # Plot management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126.5, hjust=0, size=1.5, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot Sonoma-Mendocino country line
  geom_hline(yintercept=son_mend_county, linetype="dashed", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=1.5, hjust=0) +
  # Plot sampling sites
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, size=da_ppm_avg, color=status)) +
  # geom_text(data=sample_sites, mapping=aes(x=long_dd, y=lat_dd, label=location), size=1, color="darkred") +
  # Labels
  labs(x="", y="", tag="") +
  scale_x_continuous(breaks=seq(-128,-120, 2)) +
  # Legends
  scale_color_discrete(name="Survey results") +
  scale_size_continuous(name="Mean comoic acid\ncontamination (ppm)") +
  # Crop
  coord_sf(xlim = c(-127, -120), ylim = c(35, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g


# Export plot
ggsave(g, filename=file.path(workdir, "2021nov_dcrab_da_results.png"),
       width=4.5, height=4, units="in", dpi=600)




