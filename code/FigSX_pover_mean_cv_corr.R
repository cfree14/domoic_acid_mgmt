

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(fitdistrplus)

# Directories
datadir <- "data/merged/processed"
plotdir <- "figures"

# Read data
data <- readRDS(file=file.path(datadir, "CA_OR_WA_da_survey_results.Rds"))


# Plot data
################################################################################

# A. Mean
g1 <- ggplot(data, aes(x=pover, y=da_ppm_avg)) +
  geom_point() +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="Mean contamination (ppm)", tag="A") +
  # Theme
  theme_bw()
g1

# B. log(mean)
g2 <- ggplot(data, aes(x=pover, y=meanlog)) +
  geom_point() +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="meanlog", tag="B") +
  # Theme
  theme_bw()
g2

# C. log(mean)
g3 <- ggplot(data, aes(x=pover, y=sdlog)) +
  geom_point() +
  # Labels
  labs(x="Proportion above\naction threshold",
       y="sdlog", tag="C") +
  # Theme
  theme_bw()
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)
g
