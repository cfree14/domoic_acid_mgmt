

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
outdir <- "data/california/pier_sampling/data"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outdir, "2005_2021_pier_sampling_data.Rds"))


# Format data
################################################################################

# Location key
loc_key <- data_orig %>%
  group_by(location, lat_dd, long_dd) %>%
  summarize(n=n()) %>%
  arrange(desc(lat_dd))

# Date key
dates <- seq(ymd("2014-01-01"), ymd("2021-06-30"), by="1 week")
date_key <- tibble(date_std=dates) %>%
  mutate(year=year(date_std),
         week=week(date_std)) %>%
  select(year, week, date_std)

# Data key
data_key <- purrr::map_df(loc_key$location, function(x){
  date_key %>%
    mutate(location=x) %>%
    select(location, everything())
})

# Build data
data <- data_orig %>%
  # After 2014
  filter(date >= ymd("2014-01-01") & date <= ymd("2021-06-30") & !is.na(pda_ng_ml)) %>%
  # Add standardized week
  mutate(year=year(date),
         week=week(date)) %>%
  left_join(date_key) %>%
  # Calculate max pDA by week
  group_by(location, year, week, date_std) %>%
  summarize(n=n(),
            pda_ng_ml_max=max(pda_ng_ml)) %>%
  ungroup() %>%
  # Arrange
  arrange(location, date_std)

# Expand data for plotting
data_plot <- data_key %>%
  left_join(data) %>%
  # Remove Trinidad
  filter(location!="Trinidad Pier") %>%
  # Order by location
  mutate(location=factor(location, levels=loc_key$location))

# Season key
season_key <- tibble(year1=2013:2020,
                     year2=2014:2021) %>%
  mutate(season=paste(year1, year2-2000, sep="-"),
         open_date=paste0(year1, "-11-15"),
         close_date=paste0(year2, "-06-30")) %>%
  # Recode 2014 season date
  mutate(open_date=ifelse(year1==2013, "2014-01-01", open_date)) %>%
  # Convert to date
  mutate(open_date=ymd(open_date),
         close_date=ymd(close_date))

# Export data
saveRDS(data_plot, file=file.path(outdir, "2014_2021_pda_density_by_pier_for_plotting.Rds"))


# Plot data
################################################################################

# Turn off scientific notation for plotting
options(scipen=999)

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


# Plot data
# Clarissa's threshold of evevated pDA is 0.5 ug/L = 0.5 ng/ml
# 0.5 ug/L * (1 L / 1000 mL) * (1 g / 10^6 ug) * (10^9 ng / 1 g)
ymax <- max(data_plot$pda_ng_ml_max, na.rm=T) + 0.00001
g <- ggplot() +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date), ymin=-10, ymax=ymax, fill="grey90") +
  # Lines
  geom_line(data=data_plot, mapping=aes(x=date_std, y=pda_ng_ml_max+0.00001), lwd=0.4) +
  facet_wrap(~location, ncol=1) +
  # Elevated pDA reference line
  geom_hline(yintercept=0.5, linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Particulate\ndomoic acid (ng/ml)") +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10",
                     breaks=c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10),
                     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1", "10")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS4_particulate_domoic_acid_by_ca_pier.png"),
       width=5, height=7, units="in", dpi=600)

