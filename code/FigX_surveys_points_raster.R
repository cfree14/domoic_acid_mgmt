


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

# Read zones
zones_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Read sites
sites <- readxl::read_excel("data/merged/processed/WC_dcrab_sampling_sites.xlsx", sheet=2)

# Read survey data
data_orig <- readRDS(file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))

# Read SMA polygons
sma_polys <- sf::st_read("data/washington/gis_data/processed/sma_polygons.shp")
# sma_coords <- readxl::read_excel("data/washington/gis_data/processed/SMA_coordinates.xlsx")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60


# Season info
################################################################################

# Seasons
seasons_do <- 2014:2020

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Washington season
# December 1st to September 15th
openers_wa <- paste0(seasons_do, "-12-01") %>% ymd()
closers_wa <- paste0(seasons_do+1, "-09-15") %>% ymd()
seasons_wa <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_wa,
                     close=closers_wa)

# Oregon season
# December 1st to August 14th
openers_or <- paste0(seasons_do, "-12-01") %>% ymd()
closers_or <- paste0(seasons_do+1, "-08-14") %>% ymd()
seasons_or <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_or,
                     close=closers_or)

# California-North season
# December 1st to July 15th
openers_ca_n <- paste0(seasons_do, "-12-01") %>% ymd()
closers_ca_n <- paste0(seasons_do+1, "-07-15") %>% ymd()
seasons_ca_n <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_n,
                       close=closers_ca_n)

# California-Central season
# November 15th to June 30th
openers_ca_c <- paste0(seasons_do, "-11-15") %>% ymd()
closers_ca_c <- paste0(seasons_do+1, "-06-30") %>% ymd()
seasons_ca_c <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_c,
                       close=closers_ca_c)

# Build zones and sites
################################################################################

# Build zones (for map)
##########################################

# Build zones
zones1 <- zones_orig %>%
  filter(state=="Washington") %>%
  mutate(season="2015-16 season") %>%
  select(season, everything())
zones2 <- zones_orig %>%
  mutate(season="2021-22 season") %>%
  select(season, everything())
zones_for_map <- bind_rows(zones1, zones2) %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))

zones_no_ncal_line <- zones_for_map %>%
  filter(landmark_north!="Sonoma/Mendocino County Line")

# Zone points
zone_pts <- zones_for_map %>%
  mutate(!is.na(lat_dd))

# Borders
border_n <- zones_for_map %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones_for_map %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()

# Build zones (for raster)
##########################################

# Build zones dataframe
zones_df <- zones_orig %>%
  filter(!is.na(lat_dd_north)) %>%
  select(state, lat_dd_north) %>%
  rename(y=lat_dd_north) %>%
  mutate(x1=recode(state,
                   "Washington"="2014-10-01",
                   "Oregon"="2017-11-01",
                   "California"="2020-11-01") %>% ymd(),
         x2=recode(state,
                   "Washington"="2021-09-15",
                   "Oregon"="2021-08-14",
                   "California"="2021-07-15"),
         x2=ifelse(y<38.3, "2021-06-30", x2),
         x2=ymd(x2))

# Build zones
zones1 <- zones_orig %>%
  filter(state=="Washington") %>%
  mutate(season="2015-16 season") %>%
  select(season, everything())
zones2 <- zones_orig %>%
  mutate(season="2020-21 season") %>%
  select(season, everything())
zones_for_raster <- bind_rows(zones1, zones2) %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))


# Build survey data
################################################################################

# Theshold
da_ppm_thresh <- 30

# Data
data <- data_orig %>%
  # Reduce to Dungness crab
  filter(comm_name=="Dungeness crab" & tissue=="viscera" & date >= ymd("2000-01-01")) %>%
  # Add survey id
  mutate(surveyid=paste(date, location, sep="-")) %>%
  # Summarize results by surveyid
  group_by(state, year, month, date, location, surveyid) %>%
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd),
            n=n(),
            nover=sum(da_ppm>=da_ppm_thresh),
            pover=nover/n,
            da_ppm_avg=median(da_ppm),
            type=cut(pover, breaks = c(-Inf, 0.00000001, 0.5, Inf),
                     labels=c("Clean (0% over)", "Intermediate (<50% over)", "Closed (≥50% over)"), right = F, ordered_result = T)) %>%
  ungroup() %>%
  # Filter to complete surveys
  filter(n>=5)

# Individual results
data_indiv <- data_orig %>%
  # Reduce to Dungness crab
  filter(comm_name=="Dungeness crab" & tissue=="viscera" & date >= ymd("2000-01-01")) %>%
  # Add survey id
  mutate(surveyid=paste(date, location, sep="-")) %>%
  # Filter to surveys not included
  filter(!surveyid %in% data$surveyid)

# Build call outs
stars_surveys <- matrix(data=c("1", "2016-04-15", 41.6,
                               "2", "2015-05-15", 47.65,
                               "3", "2017-03-20", 44.2,
                               "4", "2018-03-10", 43.18,
                               "4", "2019-04-22", 43.18,
                               "5", "2021-03-15", 46.95), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(date=ymd(date),
         lat_dd=as.numeric(lat_dd))


# Build raster
##########################################

# Lat breaks
range(data$lat_dd)
lat_breaks <- seq(34, 50, 0.5)
lat_labels <- zoo::rollmean(lat_breaks, k=2)

# Date breaks
range(data$date)
date_breaks <- seq(ymd("2000-01-01"), ymd("2021-03-15"), by="4 weeks")
date_labels <- zoo::rollmean(date_breaks, k=2)

# Build raster data
stats <- data %>%
  # Add lat zone
  mutate(lat_zone=cut(lat_dd, breaks=lat_breaks, labels=lat_labels)) %>%
  # Add date zone
  mutate(date_zone=cut(date, breaks=date_breaks, labels=date_labels)) %>%
  # Calculate stats
  group_by(date_zone, lat_zone) %>%
  summarize(pover_avg=mean(pover),
            pover_max=max(pover),
            pover_med=median(pover)) %>%
  ungroup() %>%
  # Format data
  mutate(lat_zone=lat_zone %>% as.character() %>% as.numeric(),
         date_zone=ymd(date_zone)) %>%
  # Filter date
  filter(date_zone>="2014-01-01")

# Build call outs
stars <- matrix(data=c("3","2016-05-01", 41.6, # large bloom
                       "2", "2019-04-01", 41.6, # medium bloom
                       "1", "2018-03-15", 41.6), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(date=ymd(date),
         lat_dd=as.numeric(lat_dd))

# Plot data
################################################################################

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

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
g1 <- ggplot(zones_for_map) +
  # Plot SMA
  geom_sf(data=sma_polys, fill="grey60", color=NA) +
  # Plot management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones_for_map, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126.5, hjust=0, size=1.5, show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot Sonoma-Mendocino country line
  geom_hline(yintercept=son_mend_county, linetype="dashed", size=0.2) +
  # Plot SMAs
  # geom_path(data=sma_coords, mapping=aes(x=long_dd, y=lat_dd, group=subunit)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot SMA labels
  geom_sf_text(data=sma_polys %>% filter(subunit!="Quinault-Split Rock to Raft River"), mapping=aes(label=sma), hjust=-0.1, size=1.5, fontface = "italic") +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=1.5, hjust=0) +
  # Plot sampling sites
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd), size=1, color="darkred") +
  # geom_text(data=sample_sites, mapping=aes(x=long_dd, y=lat_dd, label=location), size=1, color="darkred") +
  # Labels
  labs(x="", y="", tag="A") +
  scale_x_continuous(breaks=seq(-128,-120, 2)) +
  # Legends
  # scale_size_continuous(name="Mean seasonal\nlandings (mt)", range=c(0.1,6)) +
  # Crop
  coord_sf(xlim = c(-127, -120), ylim = c(35, 48)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot survey data
g2 <- ggplot(data %>% filter(date>=date_min_do),
             aes(x=date, y=lat_dd, size=da_ppm_avg, fill=pover)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Incomplete survey points
  # geom_point(data=data_indiv %>% filter(date>=date_min_do), aes(x=date, y=lat_dd), shape="x", inherit.aes = F) +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.2) +
  geom_text(data=zones_for_raster, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2021-10-01"), hjust=0, size=1, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.3) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.3) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=1.7) +
  # Survey points
  geom_point(alpha=0.8, pch=21, stroke=0.3) +
  # Plot California N/As
  annotate(geom="text", x=ymd("2015-03-15"), y=c(40.38437, 36.88437), label="N/A", color="grey30", size=1.5) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="", y="Latitude (°N)", tag="B") +
  # Legends
  scale_size_continuous(name="Median survey\ndomoic acid (ppm)", range = c(0.01, 2.5)) +
  # scale_fill_ordinal(name="Survey results") +
  scale_fill_gradientn(name="Percent above\naction threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.key.size = unit(0.3, "cm"))
g2


# Plot data
g3 <- ggplot(stats, aes(x=date_zone, y=lat_zone, fill=pover_max)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Raster
  geom_tile() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=1.7) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=1.7) +
  # Plot call outs
  geom_point(stars, mapping=aes(x=date, y=lat_dd), pch=21, fill="white", inherit.aes = F, size=3.5) +
  geom_text(stars, mapping=aes(x=date, y=lat_dd, label=id), inherit.aes = F, size=2.2) +
  # Labels
  labs(x="", y="Latitude (°N)", tag="C") +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Legend
  scale_fill_gradientn(name="Maximum\npercent above\naction threshold    ",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9],
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge plot
layout_matrix <- matrix(data=c(1,2,
                               1,3), ncol=2, byrow = T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.3,0.7))

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_surveys_points_raster.png"),
       width=6.5, height=4, units="in", dpi=600)




