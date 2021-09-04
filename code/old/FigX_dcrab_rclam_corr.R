
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
ordir <- "data/oregon/processed"
outdir <- "data/merged/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Read management zones
zones_orig <- readxl::read_excel(file.path(outdir, "WC_dcrab_da_mgmt_zones.xlsx")) %>%
  # Remove inland zones
  filter(!is.na(lat_dd_north))

# Extract management zone breaks and labels
zone_labels <- zones_orig$zone_id
zone_breaks <- c( zones_orig$lat_dd_north, zones_orig$lat_dd_south[nrow(zones_orig)])


# Build data
################################################################################

# Goal of analysis
# Calculate and correlate median domoic acid contamination
# by latitude (mgmt zones?) and time bins (2 week intervals?)

# Build data
data_all_spp <- data_orig %>%
  # Categorize spatial zones
  mutate(lat_catg=cut(lat_dd, breaks=zone_breaks, labels = zone_labels)) %>%
  # Categorize temporal zones
  mutate(jweek=week(date),
         jweek_rounded=floor(jweek/2)*2,
         time_catg=paste(year, jweek_rounded, sep="-")) %>%
  # Compute medians
  group_by(comm_name, tissue, lat_catg, time_catg) %>%
  summarize(n=n(),
            da_ppm_med=max(da_ppm, na.rm=T)) %>%
            # da_ppm_med=quantile(da_ppm, probs=0.5)) %>%
  ungroup()

# Extract razor clam
data_rclam <- data_all_spp %>%
  filter(comm_name=="Razor clam" & tissue=="meat") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_rclam=da_ppm_med)

# Extract Dungeness crab
data_dcrab <- data_all_spp %>%
  filter(comm_name=="Dungeness crab" & tissue=="viscera") %>%
  select(lat_catg, time_catg, n, da_ppm_med) %>%
  rename(da_ppm_dcrab=da_ppm_med)

# Merge data
data <- data_rclam %>%
  inner_join(data_dcrab, by=c("lat_catg", "time_catg")) %>%
  # Add state
  left_join(zones_orig %>% select(zone_id, state), by=c("lat_catg"="zone_id"))

# Plot data
g <- ggplot(data, aes(x=da_ppm_rclam, y=da_ppm_dcrab, color=state)) +
  geom_smooth(method="lm") +
  geom_point()
g


# Build data
################################################################################

# Data
surveys <- data_orig %>%
  # Reduce to Dungness crab
  filter(comm_name=="Dungeness crab" & tissue=="viscera" & date >= ymd("2000-01-01")) %>%
  # Add survey id
  mutate(surveyid=paste(date, location, sep="-")) %>%
  # Summarize results by surveyid
  group_by(state, year, month, date, location, surveyid) %>%
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd),
            n=n(),
            nover=sum(da_ppm>=30),
            pover=nover/n,
            da_ppm_avg=median(da_ppm)) %>%
  ungroup() %>%
  # Filter to complete surveys
  filter(n>=5) %>%
  # Assign a zone and week
  # Categorize spatial zones
  mutate(lat_catg=cut(lat_dd, breaks=zone_breaks, labels = zone_labels)) %>%
  # Categorize temporal zones
  mutate(jweek=week(date),
         jweek_rounded=floor(jweek/2)*2,
         time_catg=paste(year, jweek_rounded, sep="-")) %>%
  # Add razor clam
  inner_join(data_rclam, by=c("lat_catg", "time_catg")) %>%
  # over
  mutate(over=ifelse(nover>0, 1, 0))

ggplot(surveys, aes(x=da_ppm_rclam, y=pover)) +
  geom_point()

glmfit <- glm(over ~ da_ppm_rclam, data=surveys, family = "binomial")
summary(glmfit)

# plot(glmfit)

x <- 1:500
y <- predict.glm(object=glmfit, newdata = tibble(da_ppm_rclam=x), type="response")
df <- tibble(x=x,
             p_contam=y)

g <- ggplot(data=df, aes(x=x, y=p_contam)) +
  geom_line() +
  geom_point(data=surveys, mapping=aes(x=da_ppm_rclam, y=over)) +
  lims(y=c(0,1)) +
  theme_bw()
g


