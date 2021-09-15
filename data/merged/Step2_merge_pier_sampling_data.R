
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir_wa <- "data/orhab/data/processed"
datadir_or <- "data/merhab/data"
datadir_ca <- "data/california/pier_sampling/data"
outdir <- "data/merged/processed"

# Read OR data
or_orig <- readRDS(file=file.path(datadir_or, "MERHAB_2017_2021_beach_sampling_data.Rds"))

# Read WA data
wa_orig <- readRDS(file=file.path(datadir_wa, "ORHAB_2000_2020_beach_sampling_data.Rds"))

# Read CA data
ca_orig <- readRDS(file=file.path(datadir_ca, "1969_2021_pier_sampling_data_final.Rds"))


# Format data
################################################################################

# Columns names
colnames(or_orig)
colnames(wa_orig)
colnames(ca_orig)

# Format WA data
wa <- wa_orig %>%
  # Add columns
  mutate(state="Washington",
         source="ORHAB") %>%
  # Simplify
  select(source, state, site, lat_dd, long_dd,
         year, month, week, date,
         temp_c, salinity,
         pda_ng_l,
         pn_cells_l, pn_cells_l_lg, pn_cells_l_sm,
         alexandrium_cells_l, dinophysis_cells_l,
         pn_perc, pn_perc_pm, pn_perc_afh, pn_perc_pdc, pn_perc_lg, pn_perc_sm, notes) %>%
  # Rename
  rename(salinity_psu=salinity,
         pn_lg_cells_l=pn_cells_l_lg,
         pn_sm_cells_l=pn_cells_l_sm,
         pn_tot_cells_l=pn_cells_l)

# Format OR data
or <- or_orig %>%
  # Add columns
  mutate(state="Oregon",
         source="MERHAB") %>%
  # Simplify
  select(source, state, site, lat_dd, long_dd,
         year, month, week, date,
         temp_c, salinity_ppt,
         pn_tot_cells_l, pn_lg_cells_l, pn_sm_cells_l,
         pn_americana_cells_l, alexandrium_cells_l, dinophysis_cells_l,
         pda_ng_l,
         comments) %>%
  # Rename
  rename(salinity_psu=salinity_ppt,
         notes=comments)

# Format CA data
ca <- ca_orig %>%
  # Add columns
  mutate(state="California",
         source="HABMAP",
         month=month(date),
         week=week(date)) %>%
  # Simplify
  select(source, state, location, lat_dd, long_dd,
         year, month, week, date,
         temp_c, salinity,
         pda_ng_ml, tda_ng_ml, dda_ng_ml,
         pseudo_nitzschia_delicatissima_group_cells_l,
         pseudo_nitzschia_seriata_group_cells_l,
         alexandrium_spp_cells_l,
         dinophysis_spp_cells_l) %>%
  # Convert to pDA ng/ml to ng/L
  mutate(pda_ng_l=pda_ng_ml*1000,
         tda_ng_l=tda_ng_ml*1000,
         dda_ng_l=dda_ng_ml*1000) %>%
  select(-c(pda_ng_ml, tda_ng_ml, dda_ng_ml)) %>%
  # Rename
  rename(site=location,
         salinity_psu=salinity,
         pn_sm_cells_l=pseudo_nitzschia_delicatissima_group_cells_l,
         pn_lg_cells_l=pseudo_nitzschia_seriata_group_cells_l,
         alexandrium_cells_l=alexandrium_spp_cells_l,
         dinophysis_cells_l=dinophysis_spp_cells_l) %>%
  # Add a total PN column
  mutate(pn_tot_cells_l=pn_sm_cells_l+pn_lg_cells_l)


# Merge data
################################################################################

# Merge data
data <- bind_rows(ca, or, wa) %>%
  # Arrange columns
  select(source, state, site, lat_dd, long_dd,
         year, month, week, date,
         temp_c, salinity_psu,
         pda_ng_l, tda_ng_l, dda_ng_l,
         pn_tot_cells_l, pn_lg_cells_l, pn_sm_cells_l, pn_americana_cells_l,
         alexandrium_cells_l, dinophysis_cells_l,
         pn_perc, pn_perc_lg, pn_perc_sm, pn_perc_pm, pn_perc_afh, pn_perc_pdc,
         notes,
         everything()) %>%
  # Arrange rows
  arrange(state, site, date)

# Inspect
str(data)
freeR::complete(data)
colnames(data)

# Build sampling site key
site_key <- data %>%
  group_by(source, state, site, lat_dd, long_dd) %>%
  summarize(nyrs=n_distinct(year),
            nobs=n())


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CA_OR_WA_beach_pier_sampling_data_merged.Rds"))
write.csv(site_key, file=file.path(outdir, "CA_OR_WA_beach_pier_sampling_sites.csv"), row.names=F)

