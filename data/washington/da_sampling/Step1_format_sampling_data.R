

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/raw_data/Files for Chris Free"
outdir <- "data/washington/da_sampling/data"
plotdir <- "data/washington/da_sampling/figures"

# Read grid codes
grid_codes_orig <- readxl::read_excel(file.path(indir, "Grid Codes.xls"))

# Read lat/long for sites missing from grid_codes
grid_codes_missing <- readxl::read_excel(file.path(outdir, "missing_site_latlongs.xlsx"))
grid_codes_missing$site %in% grid_codes_orig$SAMPLING_SITE

# Read Pacific county
# Reading this data as an Excel sheet produced errors so I saved and read the sheet as a CSV
data1_orig <- read.csv(file.path(indir, "Coast Biotoxin Records 2000_2020_sheet1.csv"), as.is=T) %>%
  # Convert dates
  mutate(CollectDate=mdy(CollectDate) %>% as.character(),
         SubmitDate=mdy(SubmitDate) %>% as.character(),
         ReceiveDate=mdy(ReceiveDate) %>% as.character(),
         PSP.Date=mdy(PSP.Date) %>% as.character(),
         Domoic.Date=mdy(Domoic.Date) %>% as.character(),
         DSP.Date=mdy(DSP.Date) %>% as.character()) %>%
  # Make column names match others
  rename('DSP#'='DSP.',
         "PSP#"="PSP.",
         "DA#"="DA.",
         "Cert#"="Cert.",
         "PSP Result"="PSP.Result",
         "PSP Date"="PSP.Date",
         "Domoic Result"="Domoic.Result",
         "Domoic Date"="Domoic.Date",
         "DSP Result"="DSP.Result",
         "DSP Date"="DSP.Date")

# Read other counties
data2_orig <- readxl::read_excel(file.path(indir, "Coast Biotoxin Records 2000_2020.xls.xlsx"), sheet=2) # Clallam County
data3_orig <- readxl::read_excel(file.path(indir, "Coast Biotoxin Records 2000_2020.xls.xlsx"), sheet=3) # Jefferson County
data4_orig <- readxl::read_excel(file.path(indir, "Coast Biotoxin Records 2000_2020.xls.xlsx"), sheet=4) # Grays Harbor

# Check names
colnames(data1_orig)[!colnames(data1_orig)%in%colnames(data2_orig)]

# Grid codes
################################################################################

# Land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Counties
# wa <- tigris::counties(state="Washington", resolution = "500k", class="sf")

# Format data
site_key_xy <- grid_codes_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(site=sampling_site, lat_dd=lat, long_dd=long, county_id=county_code) %>%
  # Add county
  mutate(county=recode(county_id,
                       "25"="Pacific",
                       "14"="Grays Harbor",
                       "16"="Jefferson",
                       "05"="Clallam",
                       "37"="Whatcom",
                       "28"="San Juan",
                       "29"="Skagit",
                       "15"="Island",
                       "31"="Snohomish",
                       "17"="King",
                       "18"="Kitsap",
                       "23"="Mason",
                       "34"="Thurston",
                       "27"="Pierce")) %>%
  # Arrange
  select(county_id, county, waterbody, site, lat_dd, long_dd, everything()) %>%
  # Reduce to unique
  unique() %>%
  # Add missing sites
  bind_rows(grid_codes_missing)

# BE CAREFUL-Many site names are duplicated
freeR::which_duplicated(site_key_xy$site)

# Plot data
g <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # geom_sf(data=wa, fill="grey80", color="white", lwd=0.3) +
  # geom_sf_text(data=wa, mapping=aes(label=NAME), color="white") +
  # Plot sampling sites
  geom_point(data=site_key_xy, mapping=aes(x=long_dd, y=lat_dd, color=county)) +
  # Crop
  coord_sf(xlim=c(-125.5,-122), ylim=c(46, 49.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw()
g

# Export data
write.csv(site_key_xy, file=file.path(outdir, "WA_DOH_biotoxin_sampling_site_key.csv"))

# Interact with plot
#ggplotly(g)


# Build data
################################################################################

# Merge data
data_full <- bind_rows(data1_orig %>% mutate_all(as.character),
                       data2_orig %>% mutate_all(as.character),
                       data3_orig %>% mutate_all(as.character),
                       data4_orig %>% mutate_all(as.character))

# Format data
data <- data_full %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(organization=org, organization_id=cert_number,
         comm_name=species, site=site_name,
         sample_date=collect_date,
         domoic_id=da_number, domoic_ppm=domoic_result,
         psp_id=psp_number, psp_ug100g=psp_result,
         dsp_id=dsp_number, dsp_ug100g=dsp_result) %>%
  # Format organization
  mutate(organization=recode(organization,
                             "Coast Seafood Company"="Coast Seafoods Company",
                             "Bay Center Mariculture Co"="Bay Center Mariculture Co.")) %>%
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>%
  # Add scientific name
  mutate(sci_name=recode(comm_name,
                         "Barnacle"="Barnacle spp.", # Balanus glandula??? Acorn barnacle???
                         "Blue mussel"="Mytilus trossulus", # Pacific blue mussel
                         "Butter clam"="Saxidomus gigantea",
                         "California mussel"="Mytilus californianus",
                         "Cockle"="Clinocardium nuttallii",
                         "Dungeness crab"="Metacarcinus magister",
                         "Horse clam"="Tresus capax", # Gaper clam
                         "Littleneck clam"="Leukoma staminea", # Native littleneck clam
                         "Manila clam"="Ruditapes philippinarum", # Manila (littleneck) clam
                         "Olympia oyster"="Ostrea lurida",
                         "Other"="Unknown",
                         "Pacific oyster"="Crassostrea gigas",
                         "Razor clam"="Siliqua patula")) %>%
  # Recode incorrect SUBMIT dates
  mutate(submit_date=ifelse(submit_date=="1900-01-01", NA, submit_date)) %>%
  # Format all dates
  mutate(sample_date=ymd(sample_date),
         submit_date=ymd(submit_date),
         receive_date=ymd(receive_date),
         domoic_date=ymd(domoic_date),
         psp_date=ymd(psp_date),
         psp_date=ymd(psp_date)) %>%
  # Add more date info
  mutate(sample_year=year(sample_date),
         sample_month=month(sample_date)) %>%
  # Format domoic values (<0, NoTest, NTD, UNSAT)
  mutate(domoic_ppm=recode(domoic_ppm,
                           "<1"="0",
                           "NoTest"="",
                           "NTD"="0",
                           "UNSAT"=""),
         domoic_ppm=as.numeric(domoic_ppm)) %>%
  # Format PSP value (<38, NoTest, NTD, UNSAT)
  mutate(psp_ug100g=recode(psp_ug100g,
                           "<38"="0",
                           "NoTest"="",
                           "NTD"="0",
                           "UNSAT"=""),
         psp_ug100g=as.numeric(psp_ug100g)) %>%
  # Format DSP values (<1, No Test, NTD)
  mutate(dsp_ug100g=recode(dsp_ug100g,
                           "<1"="0",
                           "No Test"="",
                           "NTD"="0"),
         dsp_ug100g=as.numeric(dsp_ug100g)) %>%
  # Format tissue types
  mutate(domoic_tissue=ifelse((domoic_tissue=="" | is.na(domoic_tissue)) & !is.na(domoic_ppm), "Unknown", domoic_tissue)) %>%
  mutate(psp_tissue=ifelse((psp_tissue=="" | is.na(psp_tissue)) & !is.na(psp_ug100g), "Unknown", psp_tissue)) %>%
  mutate(dsp_tissue=ifelse((dsp_tissue=="" | is.na(dsp_tissue)) & !is.na(dsp_ug100g), "Unknown", dsp_tissue)) %>%
  # Format monitoring type
  mutate(monitoring_type=ifelse(is.na(monitoring_type), "Unknown", monitoring_type)) %>%
  # Add location info
  # Merge based on county, waterbody, and site
  left_join(site_key_xy %>% select(county, waterbody, site, lat_dd, long_dd), by=c("county", "waterbody", "site")) %>%
  # Add sample id
  # Use PSP id as overall id because it is complete and unique
  mutate(sample_id=psp_id) %>%
  # Arrange columns
  select(sample_id, sample_year, sample_month, sample_date,
         submit_date, receive_date,
         county, waterbody, site_id, site, subsite, lat_dd, long_dd,
         organization_id, organization,
         comm_name, sci_name,
         monitoring_type, sample_type, shell_shucked, fresh_frozen,
         domoic_id, domoic_date, domoic_tissue, domoic_ppm,
         psp_id, psp_date, psp_tissue, psp_ug100g,
         dsp_id, dsp_date, dsp_tissue, dsp_ug100g,
         everything())

# Inspect data
str(data)
freeR::complete(data)

# Are ids unique?
# Yes, they are all unique
freeR::which_duplicated(data$sample_id)
freeR::which_duplicated(data$domoic_id)
freeR::which_duplicated(data$psp_id)
freeR::which_duplicated(data$dsp_id)

# Location info
table(data$county)
table(data$waterbody)
table(data$site)
table(data$site_id)
table(data$subsite)

# Are all sites in the grid codes?
sites <- sort(unique(data$site))
sites[!sites %in% site_key_xy$site] # No, 10 sites do not have XY data!
sites_missing <- data %>%
  filter(is.na(lat_dd) | is.na(long_dd)) %>%
  select(county, waterbody, site, site_id) %>%
  unique()

# Collector info
table(data$organization)
table(data$organization_id)

# Sample info
table(data$monitoring_type)
table(data$sample_type)
table(data$shell_shucked)
table(data$fresh_frozen)

# Species
sort(unique(data$comm_name))
table(data$comm_name)
table(data$sci_name)

# Tissues
table(data$domoic_tissue)
table(data$psp_tissue)
table(data$dsp_tissue)

# Results
range(data$domoic_ppm, na.rm=T)
range(data$psp_ug100g, na.rm=T)
range(data$dsp_ug100g, na.rm=T)

# Dates
range(data$sample_date, na.rm=T)
range(data$submit_date, na.rm=T) # a bunch of missing values
range(data$receive_date, na.rm=T)
range(data$domoic_date, na.rm=T)
range(data$psp_date, na.rm=T)
range(data$dsp_date, na.rm=T)



# Build keys
################################################################################

# Organization key
org_key <- data %>%
  select(organization, organization_id) %>%
  unique() %>%
  arrange(organization)

# Check for duplicates
freeR::which_duplicated(org_key$organization) # duplicates
freeR::which_duplicated(org_key$organization_id)

# Site key
site_key <- data %>%
  select(county, waterbody, site, site_id, lat_dd, long_dd) %>%
  unique()

# Check for duplicates
freeR::which_duplicated(site_key$site_id)
freeR::which_duplicated(site_key$site) # Westport duplicated


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "WA_DOH_2000_2020_biotoxin_sampling_data.Rds"))
