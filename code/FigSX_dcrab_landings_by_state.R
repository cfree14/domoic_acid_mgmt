


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/merged/processed"

# Read PACFIN data
data_orig <- wcfish::pacfin_crab2


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Calculate season-state totals
  group_by(season, state) %>%
  summarize(landings_mt=sum(landings_mt),
            revenues_usd=sum(revenues_usd)) %>%
  ungroup() %>%
  # Calculate season-state proportions
  group_by(season) %>%
  mutate(landings_prop=landings_mt/sum(landings_mt),
         revenues_prop=revenues_usd/sum(revenues_usd)) %>%
  ungroup() %>%
  # Extract year
  mutate(year=substr(season, 1, 4) %>% as.numeric()) %>%
  # Arrange
  select(year, season, everything()) %>%
  # Remove most recent season
  filter(season!="2020-2021") %>%
  # Order states
  mutate(state=factor(state, levels=c("Washington", "Oregon", "California")))

# Export data
saveRDS(data, file=file.path(datadir, "PACFIN_1980_2020_dcrab_landings_by_state.Rds"))


# Plot data
################################################################################

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot landings over time
g1 <- ggplot(data, aes(x=year, y=landings_mt/1e3, fill=state)) +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=2013.5, linetype="dotted") +
  # Labels
  labs(x="Season", y="Total landings\n(1000s of mt)") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.1, 0.8))
g1

# Plot proportional landings over time
g2 <- ggplot(data, aes(x=year, y=landings_prop, fill=state)) +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=2013.5, linetype="dotted") +
  # Labels
  labs(x="Season", y="Proportional\nlandings") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Plot revenues over time
g3 <- ggplot(data, aes(x=year, y=revenues_usd/1e6, fill=state)) +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=2013.5, linetype="dotted") +
  # Labels
  labs(x="Season", y="Total revenues\n(millions of dollars)") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g3

# Plot proportional landings over time
g4 <- ggplot(data, aes(x=year, y=revenues_prop, fill=state)) +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=2013.5, linetype="dotted") +
  # Labels
  labs(x="Season", y="Proportional\nrevenues") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g4


# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=1, heights=c(0.35, 0.15, 0.35, 0.15))
g


# Export plots
ggsave(g, filename=file.path(plotdir, "FigSX_dcrab_landings_by_state.png"),
       width=6.5, height=5.5, units="in", dpi=600)


