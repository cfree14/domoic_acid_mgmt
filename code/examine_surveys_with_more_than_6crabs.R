

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
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))


# Build data
################################################################################

# Data
data <- data_orig %>%
  # Dungeness crab
  filter(comm_name=="Dungeness crab" & tissue=="viscera") %>%
  # Build survey id
  mutate(survey_id=paste(date, location, sep="-")) %>%
  # Recode
  mutate(da_ppm=ifelse(da_ppm==0, 1, da_ppm))

# Survey key
survey_results <- data %>%
  # Summarize survey results
  group_by(year, date, state, location, survey_id) %>%
  summarize(nsamples=n(),
            nover=sum(da_ppm>=30),
            pover_obs=nover/nsamples,
            da_ppm_avg=mean(da_ppm),
            da_ppm_med=median(da_ppm)) %>%
  ungroup() %>%
  # Reduce to full surveys
  filter(nsamples>=6)
