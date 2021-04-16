

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/raw_data/Files for Chris Free/Rec Closure Logs"
outdir <- "data/washington/closures/data"
plotdir <- "data/washington/closures/figures/temp"
tabledir <- "data/washington/closures/tables"
gisdir <- "data/washington/gis_data/processed"


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
  rename(sample_date=date_collected, sample_county=county, sample_species=species,
         action_date=closure_date, action_area=closure_area, action_orig=closure_history) %>%
  # Remove useless
  select(-x10) %>%
  # Format date
  mutate(action_date=ymd(action_date)) %>%
  # Format toxin
  mutate(toxin=recode(toxin,
                      "Vp?"="VP?",
                      "Vp??"="VP?")) %>%
  # Format action history
  mutate(action_orig=gsub(" to ", " for ", action_orig),
         action_orig=recode(action_orig,
                                "Open for all species besides butter clams"="Open for all species except butter clams",
                                "Open for all species except for butter clams"="Open for all species except butter clams",
                                "Open for all species except butter and varnish clams"="All species except butter/varnish clams")) %>%
  # Add action
  mutate(action=ifelse(grepl("OPEN", toupper(action_orig)), "Open", NA),
         action=ifelse(grepl("CLOSE", toupper(action_orig)) & is.na(action), "Closed", action)) %>%
  # Add action species
  mutate(action_species=gsub("Open for |Closed for ", "", action_orig) %>% stringr::str_to_sentence(),
         action_species=recode(action_species, "Razor clams only"="Razor clams")) %>%
  # Arrange
  select(year, filename, sheetname,
         action_date, action_area, action_orig, action, action_species,
         sample_date, sample_county, sample_site,
         sample_species, toxin, toxin_level, everything())

# Inspect date
str(data0211)
freeR::complete(data0211)

# Inspect
range(data0211$action_date)
# table(data0211$action_area) # lots of areas
table(data0211$action_orig)
table(data0211$action)
table(data0211$action_species)
table(data0211$sample_county)
table(data0211$sample_site)
table(data0211$sample_species)
table(data0211$toxin)


# Format 2012-2020 data
################################################################################

# Read
# This is a file that I created manually
data1220_orig <- readxl::read_excel(file.path(indir, "2012_2020_rec_closures_raw.xlsx"))

# Format data
data1220 <- data1220_orig %>%
  # Arrange
  select(year, filename, sheetname,
         action_date_orig, action_area, action_zones, action_orig,
         sample_date, sample_county, sample_site, sample_species, toxin, toxin_level, notes, everything()) %>%
  # Remove rows with no data
  mutate(n_empty=apply(., MARGIN = 1, function(x) sum(is.na(x)))) %>%
  filter(n_empty!=(ncol(.)-4)) %>%
  select(-n_empty) %>%
  # Format county (confirmed both fixes)
  mutate(sample_county=recode(sample_county,
                              "Grays HarborT"="Grays Harbor",
                              "P"="Pacific",
                              "Point Roberts"="Whatcom")) %>%
  # Format species
  mutate(sample_species=stringr::str_to_sentence(sample_species) %>% stringr::str_trim(),
         sample_species=gsub("(.*)s$", '', sample_species),
         sample_species=recode(sample_species,
                               "Blue"="Blue mussel",
                               "Butter"="Butter clam",
                               "Dungness crab viscera"="Dungeness crab",
                               "Littleneck clam and butter clam"="Littleneck/butter clam"),
         sample_species=ifelse( (is.na(sample_species) | sample_species=="") & (!is.na(toxin) | !is.na(toxin_level) | !is.na(sample_site)),
                                "Unknown", sample_species )) %>%
  # Format toxin
  mutate(toxin=ifelse(is.na(toxin) & !is.na(sample_species), "Unknown", toxin)) %>%
  # Format sample date
  mutate(sample_date=ymd(sample_date)) %>%
  # Format action (for date below)
  mutate(action_orig=recode(action_orig,
                            "44187"="12/22/20",
                            "Reopen to all species, except butter and varnish clams 1/21"="Reopen to all species, except butter and varnish clams 1/21/20",
                            "Reopened to all species, except butter and varnish clams 12/1"="Reopened to all species, except butter and varnish clams 12/1/17",
                            "Reopen to all species, except butter and varnish clams 10/5/6"="Reopen to all species, except butter and varnish clams 10/5/16")) %>%
  # Format action date
  mutate(action_date_orig=ymd(action_date_orig),
         action_date_ext=stringr::str_extract(action_orig, "\\d+/\\d+/\\d+") %>% mdy(),
         action_date=ifelse(!is.na(action_date_orig), as.character(action_date_orig), as.character(action_date_ext)) %>% ymd(),
         action_date_check=action_date_orig==action_date_ext) %>%
  select(year:action_date_orig, action_date_ext, action_date, action_date_check, action_orig, everything()) %>%
  select(-c(action_date_orig, action_date_ext, action_date_check)) %>%
  # Format action
  mutate(action_orig_simple=gsub("\\d+/\\d+/\\d+", "", action_orig) %>% gsub(", ", "", .) %>%
           gsub("  ", " ", .) %>% gsub("  ", " ", .) %>% stringr::str_trim()) %>%
  mutate(action_orig_simple=gsub("Reopen|Reopened|Reopend", "Open", action_orig_simple),
         action_orig_simple=gsub("al ", "all ", action_orig_simple),
         action_orig_simple=gsub("Open all", "Open to all", action_orig_simple),
         action_orig_simple=gsub(" for ", " to ", action_orig_simple),
         action_orig_simple=gsub(" only", "", action_orig_simple),
         action_orig_simple=gsub("ASP/DSP/PSP| . Except Bellingham Bay/Chuckanut Bay", "", action_orig_simple),
         action_orig_simple=gsub("sepcies", "species", action_orig_simple),
         action_orig_simple=gsub("speciese", "species e", action_orig_simple),
         action_orig_simple=gsub("speciesi", "species i", action_orig_simple),
         action_orig_simple=stringr::str_trim(action_orig_simple) %>% stringr::str_to_sentence(),
         action_orig_simple=recode(action_orig_simple,
                                   "Closed to all species and crab"="Closed to all species including crab",
                                   "Closed to all species of shellfish"="Closed to all species",
                                   "Closed to all species on"="Closed to all species",
                                   "Closed to crab"="Closed to all crab species",
                                   "Closed to crab and razor clams"="Closed to all crab species and razor clams",
                                   "Closed to butter and varnish clams"="Closed to butter/varnish clams",
                                   "Open to all specieexcept butter and varnish clams"="Open to all species except butter/varnish clams",
                                   "Open to all species except butter and varnish"="Open to all species except butter/varnish clams",
                                   "Open to all species except buttervarnish and geoducks"="Open to all species except butter/varnish clams and geoducks",
                                   "Open to all species except butter and varnish and geoduck clams"="Open to all species except butter/varnish clams and geoducks",
                                   "Open to all species except east sound is closed to butter and varnish clams"="Open to all species",
                                   "Open to all crab species closed to razor clams"="Open to all crab species",
                                   "Open to crab closed all other species"="Open to all crab species",
                                   "Open to crab closed to all other species"="Open to all crab species",
                                   "Open to all species except varnish and butter clams"="Open to all species except butter/varnish clams",
                                   "Open to all species except butter and varnish clams"="Open to all species except butter/varnish clams")) %>%
  # Get action and species
  mutate(action=ifelse(action_orig_simple=="", "Unknown",
                       ifelse(grepl("Open", action_orig_simple), "Open", "Closed"))) %>%
  mutate(action_species=gsub("Closed to |Open to ", "", action_orig_simple) %>% stringr::str_to_sentence()) %>%
  # Arrange
  select(year, filename, sheetname,
         action_date, action_area, action_orig, action, action_species,
         sample_date, sample_county, sample_site,
         sample_species, toxin, toxin_level, everything()) %>%
  select(-c(action_orig_simple, action_zones, notes))

# Inspect
str(data1220)
freeR::complete(data1220)

# Inspect values
range(data1220$action_date, na.rm=T)
table(data1220$action)
table(data1220$action_species)
# table(data1220$closure_area)
# table(data1220$closure_zones)
range(data1220$sample_date, na.rm=T)
table(data1220$sample_county)
# table(data1220$sample_site)
table(data1220$sample_species)
table(data1220$toxin)


# Merge and export
################################################################################

# Merge data
data <- bind_rows(data0211, data1220)

# Inspect data
range(data$action_date, na.rm=T)
table(data$action)
table(data$sample_county)
table(data$sample_species)
sort(unique(data$action_species))

# Export
saveRDS(data, file.path(outdir, "WA_DOH_2002_2020_biotoxin_closures_rec_w_sample_data.Rds"))


# Build keys
################################################################################

# File key
file_key <- data %>%
  group_by(year, filename, sheetname) %>%
  summarize(nobs=n())
write.csv(file_key, file=file.path(tabledir, "TableSX_wa_rec_closure_file_key.csv"), row.names=F)

# Species key
spp_key <- data %>%
  group_by(action_species) %>%
  summarize(nobs=n())
write.csv(spp_key, file=file.path(tabledir, "TableSX_wa_rec_closure_spp_key.csv"), row.names=F)




# # Format 2012-2020 data (old approach)
# ################################################################################
#
#
# Read data
#
# data12_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2012.xlsx"), sheet="2012") %>%
#   mutate(year=2012, filename="Rec Closure log 2012.xlsx", sheetname="2012")
#
# data13_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2013.xlsx")) %>%
#   mutate(year=2013, filename="Rec Closure log 2013.xlsx", sheetname="Sheet1") # only one sheet
#
# data14_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2014.xlsx"), sheet="2014") %>%
#   mutate(year=2014, filename="Rec Closure log 2014.xlsx", sheetname="2014")
#
# data15_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2015.xlsx"), sheet="Sheet1") %>%
#   mutate(year=2015, filename="Rec Closure log 2015.xlsx", sheetname="Sheet1") # Sheet2/Sheet3 are 2014
#
# data16_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2016.xlsx")) %>%
#   mutate(year=2016, filename="Rec Closure log 2016.xlsx", sheetname="Sheet1")# only one sheet
#
# data17_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2017.xlsx"), sheet="Sheet1") %>%
#   mutate(year=2017, filename="Rec Closure log 2017.xlsx", sheetname="Shee1")# Notes sheet has notes?
#
# data18_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2018.xlsx"), sheet="Sheet1") %>%
#   mutate(year=2018, filename="Rec Closure log 2018.xlsx", sheetname="Sheet1")# not sure what's in Sheet2
#
# data19_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2019.xlsx"), sheet="2019") %>%
#   mutate(year=2019, filename="Rec Closure log 2019.xlsx", sheetname="2019") # only one sheet
#
# data20_orig <- readxl::read_excel(file.path(indir, "Rec Closure log 2020.xlsx"), sheet="2020") %>%
#   mutate(year=2020, filename="Rec Closure log 2020.xlsx", sheetname="2020") # QuickLook20 has other
#

# # 2012 - no closure date
# data12 <- data12_orig %>%
#   # Rename
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
#   # Remove columns
#   select(-c(x9, psp, dsp, asp)) %>%
#   # Remove useless rows
#   filter(!grepl("County", sample_date) & !is.na(sample_county)) %>%
#   # Convert sample dates
#   mutate(sample_date=sample_date %>% as.numeric() %>% as.Date(., origin = "1899-12-30") %>% ymd()) %>%
#   # Extract/format closure date
#   mutate(closure_date=stringr::str_extract(closure_history, "\\d+/\\d+/\\d+") %>% mdy()) %>%
#   select(year, filename, sheetname,
#          closure_date, closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2012 - no closure date
# data12 <- data12_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
#   select(-c(x9, psp, dsp, asp)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2013
# data13 <- data13_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species) %>%
#   rename(closure_date=x9) %>%
#   select(-c(x10, psp, dsp, asp)) %>%
#   select(year, filename, sheetname,
#          closure_date, closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2015 - no closure date
# data15 <- data15_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-c(x11, psp, dsp, asp, quick_look_of_current_closed_areas, closed_to)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2016 - no closure date
# data16 <- data16_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-c(x9, x12, psp, dsp, asp, quick_look_of_current_closed_areas, closed_to)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2017 - no closure date
# data17 <- data17_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-c(x9)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2018 - no closure date
# data18 <- data18_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-c(x9, quick_look_of_current_closed_areas, closed_to)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2019 - no closure date
# data19 <- data19_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-c(x10, quick_look_of_current_closed_areas, closed_to)) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_history,
#          sample_date, sample_county, sample_site, everything())
#
# # 2020 - no closure date
# data20 <- data20_orig %>%
#   janitor::clean_names("snake") %>%
#   rename(sample_date=date_collected, sample_county=county, sample_species=species, closure_history=shellfish_closure_history) %>%
#   select(-closure_no) %>%
#   select(year, filename, sheetname,
#          closure_area, closure_zones, closure_history,
#          sample_date, sample_county, sample_site, notes, everything())
#
#
# # Merge 2012-2020 data
# ################################################################################
#
# # Merge data
# data1220_orig <- bind_rows(data12, data13,
#                             data15, data16, data17, data18, data19, data20)
#
# # Format data
# data1220 <- data1220_orig %>%
#   # Arrange
#   select(year, filename, sheetname,
#          closure_date, closure_area, closure_zones, closure_history,
#          sample_date, sample_county, sample_site, sample_species, toxin, toxin_level, notes, everything()) %>%
#   # Rename
#   rename(action_date_orig=closure_date, action_area=closure_area, action_zones=closure_zones, action_orig=closure_history) %>%
#   # Remove useless rows
#   # Rows with just counties
#   mutate(sample_date=stringr::str_trim(sample_date)) %>%
#   filter(!grepl("County", sample_date) & !grepl("^[A-Za-z]+$", sample_date) & !(sample_date %in% c("Clallam/Jefferson", "Grays Harbor", "San Juan"))) %>%
#   # Remove useless rows
#   # Rows with no data
#   mutate(n_empty=apply(., MARGIN = 1, function(x) sum(is.na(x)))) %>%
#   filter(n_empty!=(ncol(.)-4)) %>%
#   select(-n_empty) %>%
#     # Format sample date
#   # mutate(sample_date=ifelse(sample_date!="9/27/16",
#   #                           sample_date %>% as.numeric() %>% as.Date(., origin = "1899-12-30") %>% as.character(),
#   #                           "2016-09-27"),
#   #        sample_date=ymd(sample_date)) %>%
#   # select(year:sample_date, sample_date_new, everything()) %>%
#   # Format county (confirmed both fixes)
#   mutate(sample_county=recode(sample_county, "Grays HarborT"="Grays Harbor", "P"="Pacific")) %>%
#   # Format species
#   mutate(sample_species=stringr::str_to_sentence(sample_species) %>% stringr::str_trim(),
#          sample_species=gsub("(.*)s$", '', sample_species),
#          sample_species=recode(sample_species,
#                                "Blue"="Blue mussel",
#                                "Butter"="Butter clam",
#                                "Dungness crab viscera"="Dungness crab",
#                                "Littleneck clam and butter clam"="Littleneck/butter clam"),
#          sample_species=ifelse( (is.na(sample_species) | sample_species=="") & (!is.na(toxin) | !is.na(toxin_level) | !is.na(sample_site)),
#                                 "Unknown", sample_species )) %>%
#   # Format toxin
#   mutate(toxin=ifelse(is.na(toxin) & !is.na(sample_species), "Unknown", toxin)) %>%
#   # Format action date
#   mutate(action_date_orig=ymd(action_date_orig),
#          action_date_ext=stringr::str_extract(action_orig, "\\d+/\\d+/\\d+") %>% mdy(),
#          action_date=ifelse(!is.na(action_date_orig), as.character(action_date_orig), as.character(action_date_ext)) %>% ymd(),
#          action_date_check=action_date_orig==action_date_ext) %>%
#   select(year:action_date_orig, action_date_ext, action_date, action_date_check, action_orig, everything()) %>%
#   select(-c(action_date_orig, action_date_ext, action_date_check))
#
#
#
# # Inspect
# str(data1220)
# freeR::complete(data1220)
#
#
#
# range(data1220$closure_date)
# # table(data1220$closure_area)
# # table(data1220$closure_zones)
# table(data1220$sample_county) # remove s
# # table(data1220$sample_site) # remove s
# table(data1220$sample_species) # remove s
# table(data1220$toxin)



