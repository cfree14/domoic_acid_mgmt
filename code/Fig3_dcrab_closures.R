
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
closuredir <- "data/closures/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(closuredir, "2015_2020_WC_dcrab_closures.Rds"))

# Read zones
zones_orig <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")


# Stats for MS
################################################################################

# Length of OR eviseration orders
or_evis_dates <- data_orig %>%
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


# OR short closure'
or_close_dates <- data_orig %>%
  # Reduce to OR evisceration orders
  filter(date>=ymd("2017-01-01") & lat_dd < 46 & lat_dd > 42 & status=="Domoic acid delay") %>%
  # Pull dates
  pull(date) %>% unique() %>% sort()

# 1) 2017-02-02 to 2017-02-09
length(seq(ymd("2017-02-02"), ymd("2017-02-09"), by="1 day"))


# Build zones data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

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


# Format data
################################################################################

# Fix data
data <- data_orig %>%
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
stars <- matrix(data=c("A", "2016-03-01", 42.4,
                       "B", "2015-06-01", 46.9,
                       "C", "2017-03-20", 44.45,
                       "D", "2018-03-10", 43.21,
                       "D", "2019-04-10", 43.21,
                       "E", "2021-04-15", 46.95), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(date=ymd(date),
         lat_dd=as.numeric(lat_dd))


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
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Date parameters
date_min_do <- ymd("2014-01-01")
date_max_do <- ymd("2021-01-01")

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
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
  geom_point(stars, mapping=aes(x=date, y=lat_dd), pch=21, fill="white", inherit.aes = F, size=3.5) +
  geom_text(stars, mapping=aes(x=date, y=lat_dd, label=id), inherit.aes = F, size=2.2) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy"), na.translate = F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_dcrab_closures.png"),
       width=6.5, height=4, units="in", dpi=600)


