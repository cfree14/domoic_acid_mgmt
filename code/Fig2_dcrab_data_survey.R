
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

# Read zones
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Build zones dataframe
zones_df <- zones %>%
  filter(!is.na(lat_dd_north)) %>%
  select(state, lat_dd_north) %>%
  rename(y=lat_dd_north) %>%
  mutate(x1=recode(state,
                   "Washington"="2014-10-01",
                   "Oregon"="2017-11-01",
                   "California"="2020-11-01") %>% ymd(),
         x2=ymd("2021-08-14"))


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



# Stats for manuscript
################################################################################

# When did the WA end of season in 2016
data %>%
  filter(state=="Washington" & date >= "2015-01-01" & date <= "2015-10-01") %>%
  group_by(date) %>%
  summarize(n=n()) %>%
  arrange(date)

# Plot data
################################################################################

# Build call outs
stars <- matrix(data=c(1,"2016-04-15", 41.6,
                       2, "2015-05-15", 47.65,
                       3, "2017-03-20", 44.2,
                       4, "2018-03-10", 43.18,
                       4, "2019-04-22", 43.18,
                       5, "2021-03-15", 46.95), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(id=as.numeric(id),
         date=ymd(date),
         lat_dd=as.numeric(lat_dd))

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
            aes(x=date, y=lat_dd, size=da_ppm_avg, fill=pover)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Survey points
  geom_point(alpha=0.8, pch=21, stroke=0.5) +
  # Incomplete survey points
  # geom_point(data=data_indiv %>% filter(date>=date_min_do), aes(x=date, y=lat_dd), shape="x", inherit.aes = F) +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
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
  labs(x="Sample date", y="Latitude (°N)") +
  # Legends
  scale_size_continuous(name="Median survey\ndomoic acid (ppm)", range = c(0.01, 4)) +
  # scale_fill_ordinal(name="Survey results") +
  scale_fill_gradientn(name="Percent above\naction threshold", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_dcrab_data_survey.png"),
       width=6.5, height=4, units="in", dpi=600)


