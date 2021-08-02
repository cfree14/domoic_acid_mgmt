


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
wadir <- "data/washington/da_sampling/data"
ordir <- "data/oregon/processed"
cadir <- "data/california/da_sampling/data"
outdir <- "data/merged/processed"
plotdir <- "data/merged/figures"

# Read WA data
data_wa_orig <- readRDS(file.path(wadir, "WA_DOH_2000_2020_biotoxin_sampling_data.Rds"))

# Read OR data
data_or_orig <- readRDS(file.path(ordir, "ODA_2000_2020_da_sampling_data_final.Rds"))

# Read CA crustacean data
data_ca_crab_orig <- readRDS(file.path(cadir, "CDPH_2015_2021_crustacean_data.Rds"))

# Read CA bivalve data
data_ca_biv_orig <- readRDS(file.path(cadir, "CDPH_2000_2021_other_data.Rds"))


# Build data
################################################################################

# Format CA crab data
data_ca_crab <- data_ca_crab_orig %>%
  # Select columns of interest
  select(comm_name, species, date,
         area, lat_dd, long_dd,
         sample_id, da_prefix, da_ppm) %>%
  # Add columns
  mutate(state="California",
         month=month(date),
         year=year(date),
         type="wild",
         tissue="viscera") %>%
  # Rename columns
  rename(sampleid=sample_id, location=area, sci_name=species, da_oper=da_prefix) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything())

# Format CA bivalve data
data_ca_biv <- data_ca_biv_orig %>%
  # Add columns
  mutate(date=ymd(date),
         state="California",
         month=month(date)) %>%
  # Rename
  rename(sci_name=species, location=site) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything()) %>%
  select(-c(county, sample_type, nindivs)) %>%
  # Exclude spiny lobster samples
  filter(!comm_name %in% c(data_ca_crab$comm_name))

# Format OR data
data_or <- data_or_orig %>%
  # Rename
  rename(sampleid=sample_id, tissue=type) %>%
  # Add columns
  mutate(state="Oregon",
         type=ifelse(comm_name=="Dungeness crab", "wild", "unknown")) %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid, type, tissue, da_oper, da_ppm, everything()) %>%
  select(-c(source, type_orig, time))

# Format WA data
data_wa <- data_wa_orig %>%
  # Rename
  rename(sampleid=sample_id,
         year=sample_year, month=sample_month, date=sample_date,
         location=site, tissue=domoic_tissue, da_ppm=domoic_ppm) %>%
  # Add columns
  mutate(state="Washington",
         da_oper="") %>%
  # Arrange
  select(comm_name, sci_name, state, location, lat_dd, long_dd,
         year, month, date, sampleid,  tissue, da_oper, da_ppm, everything()) %>%
  select(-c(submit_date:dsp_ug100g))


# Merge data
data <- bind_rows(data_ca_crab, data_ca_biv, data_or, data_wa) %>%
  # Format common names
  mutate(comm_name=recode(comm_name,
                          "Cockle clam"="Cockle",
                          "Basket cockle"="Cockle",
                          "Native littleneck clam"="Littleneck clam")) %>%
  # Format scientific names
  mutate(sci_name=recode(sci_name, "unknown"="Unknown"),
         sci_name=ifelse(is.na(sci_name), "Unknown", sci_name)) %>%
  # Format type
  mutate(type=ifelse(is.na(type), "unknown", type)) %>%
  # Format tissue
  mutate(tissue=tolower(tissue),
         tissue=recode(tissue, "gut"="viscera", "in shell"="whole"),
         tissue=ifelse(tissue=="" | is.na(tissue), "unknown", tissue)) %>%
  # Format operator
  mutate(da_oper=ifelse(is.na(da_oper), "", da_oper)) %>%
  # Reduce to rows with DA
  filter(!is.na(da_ppm))

# Inspect data
str(data)
freeR::complete(data)

# Inspect attributes
table(data$state)
table(data$type)
table(data$tissue)
table(data$da_oper)

# Inspect species
table(data$comm_name)
table(data$sci_name)

# Species key
spp_key <- data %>%
  group_by(comm_name, sci_name) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

