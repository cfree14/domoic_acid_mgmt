
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/merhab/data"
plotdir <- "data/merhab/figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "ODFW MERHAB Beach Sampling Summary_2017-Current.xlsx"),
                                na=c("N/A", "NA"))
# data_orig <- read.csv(file=file.path(datadir, "ODFW MERHAB Beach Sampling Summary_2017-Current.csv"),
#                       na.strings=c("N/A", "NA", "", "#N/A"))

# Build data
################################################################################

# To-do
# Time is not properly formatted

# Questions
# 1) What are percent_b_bo, extract_vol_ml, dilution_factor?

# Format data
data <- data_orig %>%
  # Clean names
  janitor::clean_names("snake") %>%
  rename(region=region_north_central_south, time=time_24hr, temp_c=temp_o_c,
         pn_tot_cells_l=pseudo_nitzschia_total_cells_l,
         pn_lg_cells_l=pseudo_nitzschia_count_lg_cells_l,
         pn_sm_cells_l=pseudo_nitzschia_count_sm_cells_l,
         pn_americana_cells_l=pseudo_nitzschiaamericana_cells_l,
         pda_ng_l=h2o_elisa_p_da_ng_l) %>%
  # Format date
  mutate(date=ymd(date)) %>%
  # Format year, month, week
  mutate(year=year(date),
         month=month(date),
         week=week(date)) %>%
  # Format temperature
  mutate(temp_c=ifelse(temp_c==61.4, NA, temp_c)) %>%
  # Format pda
  mutate(pda_ng_l=recode(pda_ng_l,
                         "ND"="0",
                         "LOW"="10",
                         "HIGH"="",
                         "<100"="90")) %>%
  mutate(pda_ng_l=as.numeric(pda_ng_l)) %>%
  # Arrange
  select(region, county, site, site_code,
         year, month, week, date, time,
         tide, temp_c, salinity_ppt,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect values
range(data$date)
table(data$year)
table(data$month)
range(data$week)
table(data$region)
table(data$county)
table(data$site)
table(data$site_code)
range(data$temp_c, na.rm=T)
range(data$salinity_ppt, na.rm=T)
table(data$tide)

# Build site key
site_key <- data %>%
  group_by(region, county, site, site_code) %>%
  summarize(n=n()) %>%
  arrange(region, county, site)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "MERHAB_2017_2021_beach_sampling_data.Rds"))
write.csv(site_key, file=file.path(datadir, "MERHAB_beach_sampling_sites.csv"), row.names=F)



# Temperature
g1 <- ggplot(data, aes(x=date, y=temp_c, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Temperature (°C)") +
  # Theme
  theme_bw()
g1

# Salinity
g2 <- ggplot(data, aes(x=date, y=salinity_ppt, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Salinity (ppt)") +
  # Theme
  theme_bw()
g2

# PN Tots
g2 <- ggplot(data, aes(x=date, y=pn_tot_cells_l+1, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Total Pseudo-nitzschia density (cells/L)") +
  # Theme
  theme_bw()
g2

# pDA
g2 <- ggplot(data, aes(x=date, y=pda_ng_l, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Particulate domoic acid (ng/L)") +
  # Theme
  theme_bw()
g2




