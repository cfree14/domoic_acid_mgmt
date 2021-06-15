
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
zones <- readxl::read_excel("data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")


# Format data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

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

# Fix data
data <- data_orig %>%
  mutate(status=as.character(status),
         status=recode(status, "Whale entanglement closure"="Marine life entanglement closure"),
         status=ifelse(status=="Out-of-season", NA, status),
         status=factor(status, levels=c("Season open", "Body condition delay",
                                        "Body condition/domoic acid delay", "Domoic acid delay",
                                        "Evisceration order", "Marine life entanglement closure")))

# Build call outs
stars <- matrix(data=c(1, "2015-06-01", 46.9,
                       2, "2017-03-20", 44.45,
                       3, "2018-03-10", 43.21,
                       3, "2019-04-10", 43.21,
                       4, "2021-04-15", 46.95), ncol=3, byrow=T, dimnames = list(NULL, c("id", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(id=as.numeric(id),
         date=ymd(date),
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

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey50", size=0.2) +
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
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy"), na.translate = F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_dcrab_closures.png"),
       width=6.5, height=4, units="in", dpi=600)


