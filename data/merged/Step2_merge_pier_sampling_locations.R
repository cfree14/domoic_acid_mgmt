


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data"
datadir_ca <- "data/california/pier_sampling/data"
outdir <- "data/merged/processed"

# Read OR/WA sites (locations estimated by Chris using PNW HAB Bulletins and Google)
sites_pnw <- readxl::read_excel(file.path(datadir, "WA_OR_pn_pda_beach_sampling_sites.xlsx"))

# Read CA data
data_ca1 <- readRDS(file.path(datadir_ca, "1969_2019_pier_sampling_data.Rds"))
data_ca2 <- readRDS(file.path(datadir_ca, "2005_2021_pier_sampling_data.Rds"))


# Build sites
################################################################################

# Build CA site key
sites_ca <- data_ca1 %>%
  select(location, lat_dd, long_dd) %>%
  bind_rows(data_ca2 %>% select(location, lat_dd, long_dd)) %>%
  unique() %>%
  mutate(state="California") %>%
  select(state, location, lat_dd, long_dd) %>%
  rename(site=location)

# Merge site keys
sites <- bind_rows(sites_pnw, sites_ca) %>%
  arrange(desc(lat_dd))



# Plot data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Theme
base_theme <- theme(axis.text=element_text(size=5),
                    axis.title=element_blank(),
                    plot.subtitle=element_text(size=6),
                    plot.title=element_text(size=7),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    # Legend
                    legend.position = "none",
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot sampling sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=state), size=0.5) +
  ggrepel::geom_text_repel(data=sites, mapping=aes(x=long_dd, y=lat_dd, color=state, label=site), size=2) +
  # Labels
  labs(x="", y="", title="West Coast beach and pier monitoring", subtitle = "for Pseudo-nitzschia and particulate domoic acid") +
  scale_y_continuous(breaks=seq(32,48,2)) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(32, 48)) +
  # Theme
  theme_bw() + base_theme
g

# Export data
write.csv(sites, file=file.path(outdir, "WC_pn_pda_beach_pier_sampling_sites.csv"), row.names = F)








