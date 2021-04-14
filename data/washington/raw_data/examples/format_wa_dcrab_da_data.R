

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/da_sampling/raw"

# Read data
data_orig <- read.csv(file=file.path(indir, "20201109_20210301_domoic_acid_dcrab.csv"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Format data
  mutate(date=mdy(date)) %>%
  # Remove incomplete samples
  filter(species!="only 5 crab collected") %>%
  # Format species/type
  mutate(species=recode(species, "Dungeness"="Dungeness crab"),
         type=recode(type, "Gut"="viscera")) %>%
  # Format concentration
  mutate(da_ppm=ifelse(da_ppm%in%c("", "NTD", "<1"), 0, da_ppm) %>% as.numeric())

# Inspect data
str(data)
freeR::complete(data)
range(data$date)
table(data$agency)
table(data$location)
table(data$species)
table(data$type)
range(data$da_ppm, na.rm=T)
hist(data$da_ppm)

