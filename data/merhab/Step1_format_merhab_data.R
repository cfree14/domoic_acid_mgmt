
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/merhab/data/raw"
outdir <- "data/merhab/data"
plotdir <- "data/merhab/figures"

# Read data (as XLS - primary)
data_orig <- readxl::read_excel(file.path(indir, "ODFW MERHAB Beach Sampling Summary_2017-Current.xlsx"),
                                na=c("N/A", "NA"))

# Read data (as CSV - for time)
data_orig_csv <- read.csv(file=file.path(indir, "ODFW MERHAB Beach Sampling Summary_2017-Current.csv"),
                      na.strings=c("N/A", "NA", "", "#N/A"))

# Read site key
site_key_orig <- read.csv(file=file.path(indir, "MERHAB_beach_sampling_site_locations_from_mhunter.csv"), as.is=T)


# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Clean names
  janitor::clean_names("snake") %>%
  rename(region=region_north_central_south,
         time=time_24hr,
         temp_c=temp_o_c,
         pn_tot_cells_l=pseudo_nitzschia_total_cells_l,
         pn_lg_cells_l=pseudo_nitzschia_count_lg_cells_l,
         pn_sm_cells_l=pseudo_nitzschia_count_sm_cells_l,
         pn_americana_cells_l=pseudo_nitzschiaamericana_cells_l,
         pda_ng_l=h2o_elisa_p_da_ng_l) %>%
  # Format date
  mutate(date=ymd(date)) %>%
  # Add time
  select(-time) %>%
  bind_cols(time=data_orig_csv$Time..24hr.) %>%
  mutate(time=recode(time, "?"="")) %>%
  # Add lat/long
  left_join(site_key_orig %>% select(site_code, lat_dd, long_dd), by="site_code") %>%
  mutate(long_dd=long_dd *-1) %>%
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
  select(region, county, site, site_code, lat_dd, long_dd,
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
sort(unique(data$time))
table(data$region)
table(data$county)
table(data$site)
table(data$site_code)
range(data$temp_c, na.rm=T)
range(data$salinity_ppt, na.rm=T)
table(data$tide)

# Build site key
site_key <- data %>%
  group_by(region, county, site, site_code, lat_dd, long_dd) %>%
  summarize(n=n()) %>%
  arrange(region, county, site)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "MERHAB_2017_2021_beach_sampling_data.Rds"))
write.csv(site_key, file=file.path(outdir, "MERHAB_beach_sampling_sites.csv"), row.names=F)


# Plot checks
################################################################################

# Theme
my_theme <- theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.text=element_text(size=8),
                  axis.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position="bottom")

# USA
usa <- rnaturalearth::ne_states("United States of America", returnclass = "sf")
g <- ggplot() +
  geom_sf(data=usa, fill="grey80", color="white") +
  geom_point(data=site_key, mapping=aes(x=long_dd, y=lat_dd, size=n)) +
  geom_text(data=site_key, mapping=aes(x=long_dd, y=lat_dd, label=site),
            hjust=0, nudge_x=0.1, size=3) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="# of samples") +
  # Crop
  coord_sf(xlim=c(-121, -126), ylim=c(42,46.5)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "merhab_beach_sampling_sites.png"),
       width=5, height=6.5, units="in", dpi=600)

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

# PN large
g2 <- ggplot(data, aes(x=date, y=pn_lg_cells_l+1, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Large Pseudo-nitzschia density (cells/L)") +
  # Theme
  theme_bw()
g2

# PN small
g2 <- ggplot(data, aes(x=date, y=pn_sm_cells_l+1, color=site)) +
  geom_line() +
  # Labels
  labs(x="Sample date", y="Small Pseudo-nitzschia density (cells/L)") +
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




