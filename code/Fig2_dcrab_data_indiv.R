
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
  filter(type=="Current" & !is.na(lat_dd_north)) %>%
  select(state, lat_dd_north) %>%
  rename(y=lat_dd_north) %>%
  mutate(x1=ifelse(state=="Oregon", "2017-11-01", "2014-10-01") %>% ymd(),
         x2=ymd("2021-08-14"))

# Build data
################################################################################

# Data
data <- data_orig %>%
  filter(comm_name=="Dungeness crab" & date >= ymd("2014-01-01") &
           !is.na(date) & !is.na(lat_dd) & !is.na(da_ppm))

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

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, size=da_ppm, color=da_ppm)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=38.143562, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=38.143562, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8) +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = 38.143562, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=ymd("2014-01-01"), y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=ymd("2014-01-01"), y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=ymd("2014-01-01"), y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=ymd("2014-01-01"), y=38.143562, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-01-01"), by="1 year"), labels=2010:2021) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)") +
  scale_color_gradientn(name="Domoic acid (ppm)", colors=RColorBrewer::brewer.pal(9, "Reds")[4:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_dcrab_data_indiv.png"),
       width=6.5, height=4, units="in", dpi=600)


