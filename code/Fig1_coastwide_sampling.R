


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read/ data
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")
sample_sites <- readxl::read_excel("data/tri_state/sampling_sites/2018may_dcrab_da_sampling_sites.xlsx")

# Read proposed sites
sample_sites_ca <- readxl::read_excel("data/california/proposed_expansion/CDFW_2020_proposed_biotoxin_zones_sites.xlsx")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Dcrab landing data
# pacfin_orig  <- wcfish::pacfin_all6
# dcrab_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/landings/pacfin/processed/PACFIN_1980_2020_dungeness_crab_landings_by_port_month.Rds")


# Format data
################################################################################

# Dungeness crab
# dcrab <- pacfin_orig %>%
#   filter(comm_name=="Dungeness crab" & year>=2010) %>%
#   group_by(state, port_code, port_name) %>%
#   summarize(landings_mt=mean(landings_mt, na.rm=T)) %>%
#   ungroup() %>%
#   filter(landings_mt>0)

# Format new CA sample sites
sample_sites_ca_use <- sample_sites_ca %>%
  # Add
  mutate(state="California") %>%
  # Rename
  rename(area=port_area, location=sampling_site) %>%
  # Reduce to new sites
  filter(type=="new") %>%
  mutate(type=recode(type, "new"="Proposed")) %>%
  # Simplify
  select(state, area, location, type, long_dd, lat_dd)

# Merge sample sites
sample_sites_use <- sample_sites %>%
  select(state, area, location, type, long_dd, lat_dd) %>%
  mutate(long_dd=long_dd*-1) %>%
  bind_rows(sample_sites_ca_use) %>%
  mutate(type=recode(type, "required"="Current", "informational"="Current", "optional"="Current"))


# Plot data
################################################################################

# Add average lat
zones <- zones %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2)

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s)

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
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Plot management zones
  geom_hline(data=zones, mapping=aes(yintercept=lat_dd_north, color=type), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, color=type, label=zone_id), x=-126.5, hjust=0, size=2, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, hjust=0) +
  # ggrepel::geom_text_repel(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=2, min.segment.length = 0) +
  # Plot sampling sites
  geom_point(data=sample_sites_use, mapping=aes(x=long_dd, y=lat_dd, shape=type), size=1) +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_color_manual(name="Zone type", values=c("grey10", "grey60")) +
  scale_shape_manual(name="Site type", values=c(16, 4)) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(33, 48)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_wc_dcrab_mgmt_map.png"),
       width=4.5, height=6.5, units="in", dpi=600)
