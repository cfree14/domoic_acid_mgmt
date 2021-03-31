
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
plotdir <- "data/merged/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Read OR zones
zones_or <- readxl::read_excel(file.path(ordir, "dcrab_da_mgmt_zones.xlsx"))
zone_lats_or <- c(zones_or$lat_dd_north[1], zones_or$lat_dd_south)
zone_lats_or_df <- tibble(x1=as.Date("2017-11-01"),
                          x2=as.Date("2021-08-14"),
                          y=zone_lats_or)


# Build data
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
            da_ppm_avg=mean(da_ppm),
            type=cut(pover, breaks = c(-Inf, 0.00000001, 0.5, Inf),
                     labels=c("Clean (0% over)", "Intermediate (<50% over)", "Closed (≥50% over)"), right = F, ordered_result = T)) %>%
  ungroup()


# Season info
################################################################################

# Seasons
seasons_do <- 2014:2020

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



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))


# Plot data
g <- ggplot(data %>% filter(date>=date_min_do),
            aes(x=date, y=lat_dd, size=da_ppm_avg, fill=type)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.15, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=41.9981226, ymax=46.15, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=38.143562, ymax=41.9981226, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=38.143562, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8, pch=21) +
  # State/region lines
  geom_hline(yintercept=c(48.48, 46.15, 41.9981226), size=0.5) +
  geom_hline(yintercept = 38.143562, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.15, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=41.9981226, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=38.143562, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Management zone lines
  geom_segment(data=zone_lats_or_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Legends
  scale_size_continuous(name="Mean survey\ndomoic acid (ppm)", range = c(0.01, 4)) +
  scale_fill_ordinal(name="Survey results") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_dcrab_da_survey.png"),
       width=6.5, height=4, units="in", dpi=600)


