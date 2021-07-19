


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/merged/processed"

# Read DA sampling data
samples_orig <- readRDS(file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))

# Read landings data
landings_orig <- readRDS(file.path(datadir, "PACFIN_1980_2020_dcrab_landings_by_state.Rds"))


# Build data
################################################################################

# Calculate samples per season
samples <- samples_orig %>%
  # Reduce to Dungeness crab
  filter(comm_name=="Dungeness crab" & date>="2000-01-01") %>%
  # Add season marker
  mutate(season=ifelse(month>=10, paste(year, year+1, sep="-"), paste(year-1, year, sep="-"))) %>%
  # Summarize by state and season
  group_by(state, season) %>%
  summarize(nsamples=n()) %>%
  ungroup()

# Build dataset
data <- samples %>%
  # Add landings
  left_join(landings_orig, by=c("state", "season")) %>%
  # Calculate samples per mt
  mutate(samples_per_mt=nsamples/landings_mt,
         mt_per_sample=landings_mt/nsamples) %>%
  # Calculate samples per latitude
  mutate(lat_dd=recode(state,
                       "California"="7",
                       "Oregon"="4.25",
                       "Washington"="2.18") %>% as.numeric()) %>%
  mutate(samples_per_lat=nsamples/lat_dd,
         lat_per_sample=lat_dd/nsamples) %>%
  # Reduce to seasons of interest
  filter(year>=2014 & year <=2019) %>%
  # Order states
  mutate(state=factor(state, levels=c("Washington", "Oregon", "California")))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_blank(),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.key.size = unit(0.3, "cm"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot number of samples over time
g1 <- ggplot(data, mapping=aes(x=year, y=nsamples, color=state)) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="Season", y="Number of samples") +
  theme_bw() + base_theme +
  theme(legend.position = c(0.75, 0.85))
g1

# Plot number of samples over time
g2 <- ggplot(data, mapping=aes(x=year, y=samples_per_mt, color=state)) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="Season", y="Number of samples\nper metric ton of landings") +
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Plot number of samples over time
g3 <- ggplot(data, mapping=aes(x=year, y=samples_per_lat, color=state)) +
  geom_line() +
  geom_point() +
  # Labels
  labs(x="Season", y="Number of samples\nper latitude of coast") +
  theme_bw() + base_theme +
  theme(legend.position = "none")
g3

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_sample_density_over_time.png"),
       width=6.5, height=2.25, units="in", dpi=600)

