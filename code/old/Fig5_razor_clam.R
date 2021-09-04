
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
ordir <- "data/oregon/processed"
outdir <- "data/merged/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Read management zones
zones_orig <- readxl::read_excel(file.path(outdir, "WC_dcrab_da_mgmt_zones.xlsx")) %>%
  # Remove inland zones
  filter(!is.na(lat_dd_north))

# Extract management zone breaks and labels
zone_labels <- zones_orig$zone_id
zone_breaks <- c( zones_orig$lat_dd_north, zones_orig$lat_dd_south[nrow(zones_orig)])


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


# Build razor clam timeline data
################################################################################

# Data
data <- data_orig %>%
  filter(comm_name=="Razor clam" & date >= ymd("2014-01-01") & !is.na(date) & !is.na(lat_dd) & !is.na(da_ppm)) %>%
  # Set 0 to 1
  mutate(da_ppm=ifelse(da_ppm==0, 1, da_ppm))


# Build correlation data
################################################################################

# Goal of analysis
# Calculate and correlate median domoic acid contamination
# by latitude (mgmt zones?) and time bins (2 week intervals?)

# Build data
data_all_spp <- data_orig %>%
  # Categorize spatial zones
  mutate(lat_catg=cut(lat_dd, breaks=zone_breaks, labels = zone_labels)) %>%
  # Categorize temporal zones
  mutate(jweek=week(date),
         jweek_rounded=floor(jweek/2)*2,
         time_catg=paste(year, jweek_rounded, sep="-")) %>%
  # Compute medians
  group_by(comm_name, tissue, lat_catg, time_catg) %>%
  summarize(n=n(),
            da_ppm_med=quantile(da_ppm, probs=0.5)) %>%
  ungroup()

# Extract razor clam
data_rclam <- data_all_spp %>%
  filter(comm_name=="Razor clam" & tissue=="meat") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_rclam=da_ppm_med)

# Extract Dungeness crab
data_dcrab <- data_all_spp %>%
  filter(comm_name=="Dungeness crab" & tissue=="viscera") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_dcrab=da_ppm_med)

# Merge data
data <- data_rclam %>%
  inner_join(data_dcrab, by=c("lat_catg", "time_catg"))

# Plot data
g <- ggplot(data, aes(x=da_ppm_rclam, y=da_ppm_dcrab)) +
  geom_smooth(method="lm") +
  geom_point()
g


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x = element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom",
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

# Plot data
g1 <- ggplot(data %>% filter(comm_name=="Razor clam"),
             aes(x=date, y=lat_dd, size=da_ppm, color=da_ppm)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8) +
  # Management zone lines
  # geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
  #              inherit.aes = F, color="grey40", size=0.35) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Razor clam") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)", range = c(0.01, 4)) +
  scale_color_gradientn(name="Domoic acid (ppm)", colors=RColorBrewer::brewer.pal(9, "Reds")[4:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  guides(size=guide_legend(order=1)) +
  # Theme
  theme_bw() + base_theme
g1

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig5_other_species.png"),
       width=6.5, height=5, units="in", dpi=600)

