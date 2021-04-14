

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/da_sampling/raw/Files for Chris Free"
outdir <- "data/washington/da_sampling/processed"
plotdir <- "data/washington/da_sampling/figures"

# Read data
data <- readRDS(file=file.path(outdir, "WA_DOH_2000_2020_biotoxin_sampling_data.Rds"))


# Total sample
################################################################################

# Total sample size
ntot <- data %>%
  group_by(comm_name, sci_name) %>%
  summarize(n_da=sum(!is.na(domoic_ppm)),
            n_psp=sum(!is.na(psp_ug100g)),
            n_dsp=sum(!is.na(psp_ug100g))) %>%
  ungroup() %>%
  mutate(n_tot=n_da+n_dsp+n_psp,
         species_label=paste0(comm_name, " (", sci_name, ")")) %>%
  arrange(desc(n_tot)) %>%
  select(species_label, n_tot, n_da, n_psp, n_dsp)


# Domoic acid data
################################################################################

data_da <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")"))

# Sample size over time
################################################################################

# Total sample size
nsamples <-  data %>%
  filter(!is.na(domoic_ppm)) %>%
  group_by(comm_name, sci_name) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")")) %>%
  arrange(desc(n))

# Tissue type
####################################

# Stats
stats1 <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  group_by(sample_year, comm_name, sci_name, domoic_tissue) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")")) %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g1 <- ggplot(stats1, aes(x=sample_year, y=n, fill=domoic_tissue)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples") +
  # Legend
  scale_fill_discrete(name="Tissue type") +
  # Theme
  theme_bw()
g1

# Monitoring type
#####################################

# Stats
stats2 <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  group_by(sample_year, comm_name, sci_name, monitoring_type) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")")) %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g2 <- ggplot(stats2, aes(x=sample_year, y=n, fill=monitoring_type)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples") +
  # Legend
  scale_fill_discrete(name="Monitoring type") +
  # Theme
  theme_bw()
g2


# Fresh/frozen
#####################################

# Stats
stats3 <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  group_by(sample_year, comm_name, sci_name, fresh_frozen) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")")) %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g3 <- ggplot(stats3, aes(x=sample_year, y=n, fill=fresh_frozen)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples") +
  # Legend
  scale_fill_discrete(name="Sample condition") +
  # Theme
  theme_bw()
g3

# Fresh/frozen
#####################################

# Stats
stats4 <- data %>%
  filter(!is.na(domoic_ppm)) %>%
  group_by(sample_year, comm_name, sci_name, shell_shucked) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(species_label=paste0(comm_name, "\n(", sci_name, ")")) %>%
  mutate(species_label=factor(species_label, levels=nsamples$species_label))

# Plot
g4 <- ggplot(stats4, aes(x=sample_year, y=n, fill=shell_shucked)) +
  facet_wrap(~species_label, scales = "free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of samples") +
  # Legend
  scale_fill_discrete(name="Sample shelled/shucked") +
  # Theme
  theme_bw()
g4


# Domoic acid distribution
################################################################################

# Plot
g <- ggplot(data_da, aes(y=species_label, x=domoic_ppm)) +
  geom_boxplot() +
  # Labels
  labs(x="Domoic acid (ppm)", y="") +
  # Scale
  scale_x_continuous(trans="log2", breaks=c(1, 2.5, 5, 10, 20, 50, 100, 200)) +
  # Theme
  theme_bw()
g


# Plot map
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Plot
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot points
  geom_point(data_da, mapping=aes(x=long_dd, y=lat_dd, color=comm_name, size=domoic_ppm)) +
  # Labels
  labs(x="", y="") +
  # Legends
  scale_color_discrete(name="Species") +
  scale_size_continuous(name="Domoic acid (ppm)") +
  # Crop
  coord_sf(xlim=c(-125.5,-122), ylim=c(46, 49.2)) +
  # Theme
  theme_bw()
g


# Plot time series
################################################################################

# Plot data
g <- ggplot() +
  facet_wrap(~species_label) +
  geom_point(data=data_da, aes(x=sample_date, y=lat_dd, size=domoic_ppm, color=domoic_tissue)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  scale_color_discrete(name="Tissue type") +
  scale_size_continuous(name="Domoic acid (ppm)") +
  # Theme
  theme_bw()
g





