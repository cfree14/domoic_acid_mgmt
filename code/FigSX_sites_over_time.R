


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/tri_state/sampling_sites"


# Build data
################################################################################

# Build data
data_orig <- purrr::map_df(list.files(datadir, pattern=".xlsx"), function(x){
  fdata <- readxl::read_excel(file.path(datadir, x)) %>%
    mutate(filename=x)
})

# Format data
data <- data_orig %>%
  mutate(year=substr(filename, 1, 4) %>% as.numeric()) %>%
  select(-filename) %>%
  select(year, everything())


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=year, y=lat_dd, color=type)) +
  geom_point() +
  geom_hline(yintercept=c(42, 46.2)) + # I'm guessing at lats here
  labs(x="Year", y="Latitude (°N)") +
  scale_color_discrete(name="Site type") +
  theme_bw()
g
