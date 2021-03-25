

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
workdir <- "data/oregon/intermediate"
outdir <- "data/oregon/processed"
plotdir <- "data/oregon/figures"

# Export data
data1_orig <- readRDS(file=file.path(outdir, "ODA_1999_2015_bivalve_data.Rds"))
data2_orig <- readRDS(file=file.path(outdir, "ODA_2010_2020_all_species_data.Rds"))

# Format data
################################################################################

data1 <- data1_orig %>%
  # Rename
  rename(sci_name=species, ) %>%
  # Add columns
  mutate(source="Clams/mussels only",
         year=year(date),
         month=month(date),
         type="not specified",
         time=NA) %>%
  # Add sampleid
  group_by(date, comm_name) %>%
  mutate(sample_id=ifelse(comm_name=="Razor clam", paste0(date, "-RC-", 1:n()), paste0(date, "-CM-", 1:n()))) %>%
  ungroup() %>%
  # Select
  select(source, comm_name, sci_name, type, year, month, date, time, location, lat_dd, long_dd, sample_id, type, da_oper, da_ppm)

anyDuplicated(data1$sample_id)

# Format
data2 <- data2_orig %>%
  # Mutate
  mutate(source="All species") %>%
  # Select
  select(source, comm_name, sci_name, type, year, month, date, time, location, lat_dd, long_dd, sample_id, type, quantity_operator, quantity) %>%
  # Rename
  rename(da_ppm=quantity, da_oper=quantity_operator)


# Merge
data_full <- bind_rows(data1, data2)


# Inspect overlap
################################################################################

# Data to check
data_check <- data_full %>%
  filter(comm_name %in% c("California mussel", "Razor clam"))

# Plot data
g <- ggplot(data_check, aes(x=date, y=lat_dd, shape=source, size=source, color=source)) +
  facet_wrap(~comm_name, ncol=1) +
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Limits
  geom_vline(xintercept=c(ymd("2010-01-01"), ymd("2016-01-01")), linetype="dashed") +
  scale_x_date(limits = c(ymd("2009-06-01"), ymd("2016-06-01"))) +
  # Legend
  scale_shape_manual("Source data", values=c(21, 16)) +
  scale_size_manual("Souce data", values=c(2,1), guide=F) +
  scale_color_manual("Souce data", values=c("black", "red"), guide=F) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=8),
        legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        strip.text=element_text(size=7),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g

# Export
ggsave(g, filename=file.path(plotdir, "OR_data_overlap.png"),
       width=6.5, height=5, units="in", dpi=600)


# Format data
################################################################################

# Use clam/mussel data before 2010-01-01
data1_use <- data1 %>%
  filter(date < ymd("2010-01-01"))

# Merge
data <- bind_rows(data1_use, data2) %>%
  arrange(comm_name, date, location, sample_id)

# Export data
saveRDS(data, file.path(outdir, "ODA_2000_2020_da_sampling_data_final.Rds"))
