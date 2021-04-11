

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/da_sampling/raw/Files for Chris Free/Rec Closure Logs"
outdir <- "data/washington/da_sampling/processed"
plotdir <- "data/washington/da_sampling/figures"

# Read data

data12_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2012.xlsx"), sheet="2012") %>%
  mutate(year=2012, filename="Rec Closure log 2012.xlsx", sheetname="2012")

data13_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2013.xlsx")) %>%
  mutate(year=2013, filename="Rec Closure log 2013.xlsx", sheetname="Sheet1") # only one sheet

data14_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2014.xlsx"), sheet="2014") %>%
  mutate(year=2014, filename="Rec Closure log 2014.xlsx", sheetname="2014")

data15_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2015.xlsx"), sheet="Sheet1") %>%
  mutate(year=2015, filename="Rec Closure log 2015.xlsx", sheetname="Sheet1") # Sheet2/Sheet3 are 2014

data16_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2016.xlsx")) %>%
  mutate(year=2016, filename="Rec Closure log 2016.xlsx", sheetname="Sheet1")# only one sheet

data17_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2017.xlsx"), sheet="Sheet1") %>%
  mutate(year=2017, filename="Rec Closure log 2017.xlsx", sheetname="Shee1")# Notes sheet has notes?

data18_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2018.xlsx"), sheet="Sheet1") %>%
  mutate(year=2018, filename="Rec Closure log 2018.xlsx", sheetname="Sheet1")# not sure what's in Sheet2

data19_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2019.xlsx"), sheet="2019") %>%
  mutate(year=2019, filename="Rec Closure log 2019.xlsx", sheetname="2019") # only one sheet

data20_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2020.xlsx"), sheet="2020") %>%
  mutate(year=2020, filename="Rec Closure log 2020.xlsx", sheetname="2020") # QuickLook20 has other


# Format 2002-2011 data
################################################################################

# Read data
data0211_orig <- purrr:::map_df(2002:2011, function(x){
  fdata_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2002-2011.xlsx"), sheet=x %>% as.character()) %>%
    mutate(year=x,
           filename="Rec Closure log 2002-2011.xlsx",
           sheetname=x %>% as.character())
})

# Format data
data0211 <- data0211_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
  # Remove useless
  select(-x10) %>%
  # Format date
  mutate(closure_date=ymd(closure_date)) %>%
  # Format toxin
  mutate(toxin=recode(toxin,
                      "Vp?"="VP?",
                      "Vp??"="VP?")) %>%
  # Arrange
  select(year, filename, sheetname,
         closure_date, closure_area, closure_history,
         sample_date, sample_county, sample_site,
         sample_species, toxin, toxin_level, everything())

# Inspect date
str(data0211)
freeR::complete(data0211)

# Inspect
range(data0211$closure_date)
table(data0211$closure_area)
table(data0211$closure_history)
table(data0211$sample_county)
table(data0211$sample_site)
table(data0211$sample_species)
table(data0211$toxin)
table(data0211$closure_date)


# Format 2012-2020 data
################################################################################

# 2012 - no closure date
data12 <- data12_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
  select(-c(x9, psp, dsp, asp)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2013
data13 <- data13_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
  rename(closure_date=x9) %>%
  select(-c(x10, psp, dsp, asp)) %>%
  select(year, filename, sheetname,
         closure_date, closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2015 - no closure date
data15 <- data15_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-c(x11, psp, dsp, asp, quick_look_of_current_closed_areas, closed_to)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2016 - no closure date
data16 <- data16_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-c(x9, x12, psp, dsp, asp, quick_look_of_current_closed_areas, closed_to)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2017 - no closure date
data17 <- data17_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-c(x9)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2018 - no closure date
data18 <- data18_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-c(x9, quick_look_of_current_closed_areas, closed_to)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2019 - no closure date
data19 <- data19_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-c(x10, quick_look_of_current_closed_areas, closed_to)) %>%
  select(year, filename, sheetname,
         closure_area, closure_history,
         sample_date, sample_county, sample_site, everything())

# 2020 - no closure date
data20 <- data20_orig %>%
  janitor::clean_names("snake") %>%
  rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
  select(-closure_no) %>%
  select(year, filename, sheetname,
         closure_area, closure_zones, closure_history,
         sample_date, sample_county, sample_site, notes, everything())


# Merge 2012-2020 data
################################################################################

# Merge data
data_1220_orig <- bind_rows(data12, data13,
                            data15, data16, data17, data18, data19, data20)

# Format data
data_1220 <- data_1220_orig %>%
  # Arrange
  select(year, filename, sheetname,
         closure_date, closure_area, closure_zones, closure_history,
         sample_date, sample_county, sample_site, sample_species, toxin, toxin_level, notes, everything()) %>%
  # Format
  mutate(sample_species=stringr::str_to_sentence(sample_species))

# Inspect
table(data_1220$sample_county) # remove s
table(data_1220$sample_species) # remove s
table(data_1220$toxin)



