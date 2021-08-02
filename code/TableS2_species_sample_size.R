


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/merged/processed"
plotdir <- "figures"
tabledir <- "tables"

# Read DA sampling data
samples_orig <- readRDS(file.path(datadir, "CA_OR_WA_da_sampling_data.Rds"))



# Buid data
################################################################################

# Build data
stats <- samples_orig %>%
  # Post-2014
  filter(date >= "2014-01-01") %>%
  # Summarize
  group_by(comm_name, sci_name) %>%
  summarize(states=paste(sort(unique(state)), collapse=", "),
            n=n()) %>%
  ungroup() %>%
  # Arrange
  arrange(desc(n)) %>%
  # Recode state
  mutate(states=recode(states,
                       "California"="CA",
                       "California, Oregon"="CA/OR",
                       "California, Oregon, Washington"="CA/OR/WA",
                       "California, Washington"="CA/WA",
                       "Oregon"="OR",
                       "Washington"="WA",
                       "Oregon, Washington"="OR/WA")) %>%
  # Add label
  mutate(species_label=paste0(comm_name, " (", sci_name, ")")) %>%
  # Simplify label
  mutate(species_label=gsub(" \\(Unknown\\)", "", species_label)) %>%
  # Arrange
  select(species_label, states, n)

# Inspect
table(stats$states)

# Export
write.csv(stats, file=file.path(tabledir, "TableS2_species_sample_size.csv"), row.names=F)
