

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
outdir <- "data/california/pier_sampling/data"
plotdir <- "data/california/pier_sampling/figures"

# Read data
data_orig <- readRDS(file.path(outdir, "2005_2021_pier_sampling_data.Rds"))

# Format data
################################################################################

# Location key
loc_key <- data_orig %>%
  group_by(location, lat_dd, long_dd) %>%
  summarize(n=n()) %>%
  arrange(desc(lat_dd))

# Format data
data <- data_orig %>%
  # After 2014
  filter(date >= ymd("2014-01-01") & date <= ymd("2021-07-15") & !is.na(pda_ng_ml)) %>%
  # Order north to south
  mutate(location=factor(location, levels=loc_key$location))

# Season key
season_key <- tibble(year1=2013:2020,
                     year2=2014:2021) %>%
  mutate(season=paste(year1, year2-2000, sep="-"),
         open_date=paste0(year1, "-11-15"),
         close_date=paste0(year2, "-07-15")) %>%
  # Recode 2014 season date
  mutate(open_date=ifelse(year1==2013, "2014-01-01", open_date)) %>%
  # COnvert to date
  mutate(open_date=ymd(open_date),
         close_date=ymd(close_date))

# Plot data
################################################################################

# Turn off scientific notation for plotting
options(scipen=999)

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


# Plot data
ymax <- max(data$pda_ng_ml) + 0.00001
g <- ggplot(data, aes(x=date, y=pda_ng_ml+0.00001)) +
  facet_wrap(~location, ncol=1) +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date, group=season), ymin=-10, ymax=ymax, fill="grey90", inherit.aes = F) +
  # Lines
  geom_line(lwd=0.4) +
  # Labels
  labs(x="Date", y="Particulate\ndomoic acid (ng/ml)") +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10", breaks=c(0.00001, 0.0001, 0.001, 0.1, 1, 10)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_2014_2021_pda_at_piers.png"),
       width=5, height=7, units="in", dpi=600)


# Build data
################################################################################

# Build data
data2 <- data_orig %>%
  # Simplify
  select(location, date, time, sample_id, pseudo_nitzschia_delicatissima_group_cells_l, pseudo_nitzschia_seriata_group_cells_l) %>%
  # Gather
  gather(key="pn_group", value="cells_l", 5:6) %>%
  mutate(pn_group=recode(pn_group,
                         "pseudo_nitzschia_seriata_group_cells_l"="Seriata",
                         "pseudo_nitzschia_delicatissima_group_cells_l"="Delicatissima")) %>%
  # Reduce
  filter(date >= ymd("2014-01-01") & date <= ymd("2021-07-15")) %>%
  # Order north to south
  mutate(location=factor(location, levels=loc_key$location))

#Plot data
g2 <- ggplot(data2, aes(x=date, y=cells_l+1, color=pn_group)) +
  facet_wrap(~location, ncol=1) +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date, group=season), ymin=-10, ymax=ymax, fill="grey90", inherit.aes = F) +
  # Lines
  geom_point() +
  # Labels
  labs(x="Date", y="Density (cells/L)") +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10", breaks=c(1,10,100,1000,10000,100000)) +
  # Legend
  scale_color_discrete("Pseudo-nitzschia group") +
  # Theme
  theme_bw() + my_theme + theme(legend.position="bottom")
g2

