
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/merged/processed"
closuredir <- "data/closures/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))
closures_orig <- readRDS(file.path(closuredir, "2015_2020_WC_dcrab_closures.Rds"))

# Read zones
zones_orig <- readxl::read_excel(file.path(datadir, "WC_dcrab_da_mgmt_zones.xlsx"))

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





# Build zones data
################################################################################

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
zones <- bind_rows(zones1, zones2) %>%
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


# Build closures data
################################################################################

# Fix data
closures <- closures_orig %>%
  mutate(status=as.character(status),
         # Rename whale closures
         status=recode(status, "Whale entanglement closure"="Marine life entanglement closure"),
         # Fix out of season
         status=ifelse(status=="Out-of-season", NA, status),
         status=ifelse(date>ymd("2021-08-14") & lat_dd<46.25000 & lat_dd>42.00000, "Out-of-season", status),
         status=ifelse(date>ymd("2021-07-15") & lat_dd<42.00000 & lat_dd>son_mend_county, "Out-of-season", status),
         status=ifelse(date>ymd("2021-06-30") & lat_dd<son_mend_county, "Out-of-season", status),
         # Factor
         status=factor(status, levels=c("Season open", "Body condition delay",
                                        "Body condition/domoic acid delay", "Domoic acid delay",
                                        "Evisceration order", "Marine life entanglement closure")))

# Build call outs
stars_closures <- matrix(data=c("1", "2016-03-01", 42.4,
                       "2", "2015-06-01", 46.9,
                       "3", "2017-03-20", 44.45,
                       "4", "2018-03-10", 43.21,
                       "4", "2019-04-10", 43.21,
                       "5", "2021-04-15", 46.95), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(date=ymd(date),
         lat_dd=as.numeric(lat_dd))


# Stats for manuscript
################################################################################

# When did the WA end of season in 2016
data %>%
  filter(state=="Washington" & date >= "2015-01-01" & date <= "2015-10-01") %>%
  group_by(date) %>%
  summarize(n=n()) %>%
  arrange(date)

# Length of OR eviseration orders
or_evis_dates <- closures_orig %>%
  # Reduce to OR evisceration orders
  filter(date>=ymd("2018-01-01") & lat_dd < 46 & lat_dd > 42 & status=="Evisceration order") %>%
  # Pull dates
  pull(date) %>% unique() %>% sort()

# 1) 2018-02-16 to 2018-03-04
# 2) 2019-02-14 to 2019-03-27
# 3) 2019-05-10 to 2019-05-23

length(seq(ymd("2018-02-16"), ymd("2018-03-04"), by="1 day"))
length(seq(ymd("2019-02-14"), ymd("2019-03-27"), by="1 day"))
length(seq(ymd("2019-05-10"), ymd("2019-05-23"), by="1 day"))

# OR short closure
or_close_dates <- closures_orig %>%
  # Reduce to OR evisceration orders
  filter(date>=ymd("2017-01-01") & lat_dd < 46 & lat_dd > 42 & status=="Domoic acid delay") %>%
  # Pull dates
  pull(date) %>% unique() %>% sort()

# 1) 2017-02-02 to 2017-02-09
length(seq(ymd("2017-02-02"), ymd("2017-02-09"), by="1 day"))


# Plot data
################################################################################

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot survey data
g1 <- ggplot(data %>% filter(date>=date_min_do),
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
               inherit.aes = F, color="grey40", size=0.35) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2021-10-01"), hjust=0, size=1.5, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Survey points
  geom_point(alpha=0.8, pch=21, stroke=0.3) +
  # Plot call outs
  geom_point(stars_surveys, mapping=aes(x=date, y=lat_dd), pch=21, fill="white", inherit.aes = F, size=3.5) +
  geom_text(stars_surveys, mapping=aes(x=date, y=lat_dd, label=id), inherit.aes = F, size=2.2) +
  # Plot California N/As
  annotate(geom="text", x=ymd("2015-03-15"), y=c(40.38437, 36.88437), label="N/A", color="grey30", size=2) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", tag="A") +
  # Legends
  scale_size_continuous(name="Median survey\ndomoic acid (ppm)                         ", range = c(0.01, 4)) +
  # scale_fill_ordinal(name="Survey results") +
  scale_fill_gradientn(name="Percent above\naction threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot closures
g2 <- ggplot(closures, aes(x=date, y=lat_dd, fill=status)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Plot raster
  geom_raster() +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey50", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2021-10-01"), hjust=0, size=1.4, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot call outs
  geom_point(stars_closures, mapping=aes(x=date, y=lat_dd), pch=21, fill="white", inherit.aes = F, size=3.5) +
  geom_text(stars_closures, mapping=aes(x=date, y=lat_dd, label=id), inherit.aes = F, size=2.2) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy"), na.translate = F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, nrow=2)


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_survey_closures_merge.png"),
       width=6.5, height=6.5, units="in", dpi=600)


