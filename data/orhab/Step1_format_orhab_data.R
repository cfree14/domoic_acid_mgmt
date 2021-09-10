
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/orhab/data/raw"
outdir <- "data/orhab/data/processed"
plotdir <- "data/orhab/figures"


# Site key
################################################################################

# Read data
sites_orig <- readxl::read_excel(file.path(indir, "Beach_Site_Coordinates.xls"))

# Format data
sites <- sites_orig %>%
  # Simplify
  select(Station, Lat_DD, Lon_DD) %>%
  # Rename
  setNames(c("station", "lat_dd", "long_dd"))

# Plot sites
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
canada <- rnaturalearth::ne_states(country="Canada", returnclass = "sf")
g <- ggplot() +
  geom_sf(data=usa, color="white", fill="grey80") +
  geom_sf(data=canada, color="white", fill="grey80") +
  # Stations
  geom_point(data=sites, mapping=aes(x=long_dd, y=lat_dd)) +
  geom_text(data=sites, mapping=aes(x=long_dd, y=lat_dd, label=station), hjust=0, nudge_x = 0.05) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(-125, -123), ylim=c(46.25, 48.5)) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g

# Export data
write.csv(sites, file=file.path(outdir, "ORHAB_beach_sampling_sites.csv"), row.names = F)


# Merge data
################################################################################

# Files
files2merge <- list.files(file.path(indir, "ORHAB2000_manually_formatted"))[1:20]

# Merge data
x <- files2merge[10]
data_orig <- purrr::map_df(files2merge, function(x){

  # Number of sheets
  sheets <- readxl::excel_sheets(file.path(indir, "ORHAB2000_manually_formatted", x))
  nsheets <- length(sheets)

  # Read sheet data
  # for(y in 1:nsheets){
  data_file <- purrr::map_df(1:nsheets, function(y){

    # Try to read
    data_sheet_orig <- try(readxl::read_excel(file.path(indir, "ORHAB2000_manually_formatted", x), sheet=y))

    # If read successfully
    if(!inherits(data_sheet_orig, "try-error")){
      data_sheet <- readxl::read_excel(file.path(indir, "ORHAB2000_manually_formatted", x), sheet=y, col_types="text",
                                       na=c("NA", "na", "N/A", "n/a", "ND"), trim_ws=T) %>%
        # Convert to character
        mutate_all(as.character) %>%
        # Add filename/sheet metadata
        mutate(filename=x,
               sheet=sheets[y]) %>%
        # Arrange
        select(filename, sheet, everything())
    }else{
      data_sheet <- tibble()
    }

  }) # finish sheets

}) # finish files


# Format data
################################################################################

# Things I haven't done:
# 1) Standardized times
# 2) Standardized dominant species
# 3) Standardized weather, tides, wind, swell

# Format data
data <- data_orig %>%
  # Format date
  mutate(date=date %>% as.numeric() %>% as.Date(., origin="1899-12-30"),
         year=year(date),
         month=month(date),
         week=week(date)) %>%
  # Check against file year
  mutate(year_file=gsub(".xls|.xlsx|ORHAB", "", filename) %>% as.numeric()) %>%
  select(-year_file) %>%
  # Format site
  mutate(site=recode(site,
                     # La Push 1st beach
                     "La Push - First Beach"="La Push - First Beach",
                     "La Push First Beach"="La Push - First Beach",
                     "First Beach, La Push"="La Push - First Beach",
                     "La Push, First Beach"="La Push - First Beach",
                     "La Push, Beach 1" = "La Push - First Beach",
                     # La Push 2nd beach
                     "La Push Second Beach"="La Push - Second Beach",
                     "La Push, Second Beach"="La Push - Second Beach",
                     "Second Beach, La Push"="La Push - Second Beach",
                     # Hobuck Beach
                     "Hobuck"="Hobuck Beach",
                     "Hobuck Beach, Makah Bay"="Hobuck Beach", # check
                     "Holbuck"="Hobuck Beach",
                     # Quinault Beach
                     "Quilault Beach"="Quinault Beach",
                     "Quinault"="Quinault Beach",
                     # Neah Bay
                     "Nemah"="Neah Bay",
                     "Neah Bay Marina"="Neah Bay",
                     # Other
                     "RaftRiver"="Raft River",
                     "THB"="Twin Harbors",
                     "Lone Tree Oyster LTO"="Lone Tree Oyster",
                     "Long island green maker 13"="Long Island - Green Maker 13",
                     "north Long Island Naselle channel"="Long Island - Naselle Channel",
                     # Westport Marina
                     "WES"="Westport Marina",
                     "Westport WES"="Westport Marina",
                     "Westport"="Westport Marina",
                     # Tokeland
                     "TOK"="Tokeland Marina",
                     "Toke Point"="Tokeland Marina",
                     "Toke Point TOK"="Tokeland Marina",
                     "Tokeland"="Tokeland Marina",
                     # Long Beach
                     "LGB"="Long Beach",
                     "LB"="Long Beach",
                     # MocRocks
                     "MocRocks"="Mocrocks Beach",
                     "Mocrocks"="Mocrocks Beach")) %>%
  # Format temperature
  mutate(temp_c=ifelse(temp_c %in% c("not", "on mooring"), NA, temp_c),
                       temp_c=as.numeric(temp_c)) %>%
  # Format salinity
  mutate(salinity=recode(salinity,
                         "31..7"="31.7",
                         "204"="20.4",
                         "2402"="24.0"),
         salinity=ifelse(salinity %in% c("on mooring", "working"), NA, salinity),
         salinity=as.numeric(salinity)) %>%
  # Format chlorophyll
  mutate(chl_a_ug_l=as.numeric(chl_a_ug_l)) %>%
  # Format conductivity
  mutate(conductivity=as.numeric(conductivity)) %>%
  # Format pH
  mutate(ph=as.numeric(ph)) %>%
  # Format DO
  mutate(do=as.numeric(do),
         do_mg_l=recode(do_mg_l, "97"="9.7"),
         do_mg_l=as.numeric(do_mg_l)) %>%
  # Format ORP (whatever that is)
  mutate(orp=as.numeric(orp)) %>%
  # Format toxin number
  mutate(toxin_filters_n=as.numeric(toxin_filters_n)) %>%
  # Format PN densities
  mutate(pn_cells_l=recode(pn_cells_l, "154,l000"="154000"),
         pn_cells_l=as.numeric(pn_cells_l),
         pn_cells_l_sm=as.numeric(pn_cells_l_sm),
         pn_cells_l_lg=as.numeric(pn_cells_l_lg)) %>%
  # Format other densities
  mutate(alexandrium_cells_l=as.numeric(alexandrium_cells_l),
         dinophysis_cells_l=as.numeric(dinophysis_cells_l),
         akashiwo_sanguinea_cells_l=as.numeric(akashiwo_sanguinea_cells_l)) %>%
  # Format PN percentages (easy ones)
  rename(pn_perc_pdc=pn_perc_pdd) %>%
  mutate(pn_perc_pm=as.numeric(pn_perc_pm),
         pn_perc_afh=as.numeric(pn_perc_afh),
         pn_perc_pds=as.numeric(pn_perc_pdc)) %>%
  # Formtat PN percentages (hard ones): pn_perc, pn_perc_sm, pn_perc_lg
  mutate(pn_perc_lg=recode(pn_perc_lg, "<1"="1"),
         pn_perc_lg=as.numeric(pn_perc_lg),
         pn_perc_sm=recode(pn_perc_sm, ">99"="99"),
         pn_perc_sm=as.numeric(pn_perc_sm),
         pn_perc=recode(pn_perc,
                        "<1"="1",
                        "?"="",
                        "80-90"="85",
                        "15-20"="17.5",
                        "20-25"="22.5",
                        "1>"="1",
                        ">1"="1",
                        ",1"="1",
                        "<5"="5",
                        "<3"="3",
                        "<2"="2",
                        ">2"="2",
                        "<1%"="1",
                        ">1%"="1",
                        "< 1%"="1"),
         pn_perc=as.numeric(pn_perc)) %>%
  # Merge pDA values
  # (I don't understand why these didn't merge properly)
  mutate(pda_ng_l=ifelse(!is.na(pda_ng_l), pda_ng_l, pda_ng_l...28),
         pda_ng_l=ifelse(!is.na(pda_ng_l), pda_ng_l, pda_ng_l...31)) %>%
  select(-c(pda_ng_l...28, pda_ng_l...31)) %>%
  # Convert pDA values
  mutate(pda_ng_l=recode(pda_ng_l,
                         "<390"="390",
                         "high"="",
                         "low"="",
                         "<100"="100",
                         "<132"="132",
                         "<900"="900",
                         "<1,000"="1000",
                         ">600"="600",
                         "ntd"="0",
                         "HIGH"="",
                         "18.3 ng/L"="18.3",
                         "NTD"="0",
                         "-"="",
                         "12ng/L"="12", ),
         pda_ng_l=as.numeric(pda_ng_l)) %>%
  # Format lat/long/depth
  mutate(lat_dd=recode(lat_dd,
                       "47 53.1"=47+53.1/60),
         lat_dd=as.numeric(lat_dd),
         long_dd=recode(long_dd,
                       "124 38.88"=124+38.88/60),
         long_dd=as.numeric(long_dd),
         depth_m=recode(depth_m, "surface"="0"),
         depth_m=as.numeric(depth_m)) %>%
  # Fix wind
  mutate(wind_kts=stringr::str_trim(wind_kts),
         wind_knts=stringr::str_trim(wind_knts),
         wind_kts=ifelse(!is.na(wind_kts), wind_kts, wind_knts)) %>%
  select(-wind_knts) %>%
  # Format wind
  mutate(wind_kts=toupper(wind_kts),
         wind_kts=gsub("  ", " ", wind_kts) %>% gsub("  ", " ", .),
         wind_kts=gsub("KTS", "", wind_kts),
         wind_kts=stringr::str_trim(wind_kts)) %>%
  # Format swell
  # You abandoned this beceause the recode wasn't working for some characters, problem with crazy non-traditional space and hyphen formats
  mutate(swell_ft=gsub("ft.|ft .|ft,|ft|fr.|t.|f.t|FT", "", swell_ft),
         swell_ft=gsub("chop|flat|Gui|n/a|na|calm|1hop||\\?|st|\\+", "", swell_ft),
         swell_ft=recode(swell_ft,
                         "< 1"="1",
                         "<1"="1",
                         "<10"="10",
                         ">10"="10",
                         "0-5"="2.5",
                         "1  2"="1.5",
                         "1-2"="1.5",
                         "1-3"="2",
                         "10 0"="10",
                         "10 12"="11",
                         "10-12"="11",
                         "12-15"="13.5",
                         "2-3"="2.5",
                         "3-4"="3.5",
                         "3-5"="4",
                         "4-5"="4.5",
                         "6-7"="6.5",
                         "6-8"="7",
                         "7-8"="7.5",
                         "8-10"="9",
                         "9-10"="9.5"),
         swell_ft=trimws(swell_ft)) %>%
  # Remove columns
  select(-c(unknown1, unknown2, unknown3, unknown_us_cm)) %>%
  select(-c(counter_inits, chl_inits, toxin_inits, sampler_inits, tower_inits)) %>%
  # Remove empty columns
  select(-c(pda_ng_l...30,  ...29)) %>%
  # Arrange
  select(filename, sheet, site,
         # Date/time
         year, month, week, date,
         time,
         # Lat/long/depth
         lat_dd, long_dd, depth_m,
         # Ids
         bottle_id,
         tow_id,
         water_id,
         chlorophyll_id,
         toxin_id,
         toxin_filters_n,
         sample_notes,
         # Oceanography
         temp_c,
         salinity,
         chl_a_ug_l,
         ph,
         do,
         do_mg_l,
         conductivity,
         orp,
         # Cell counts
         pn_cells_l,
         pn_cells_l_lg,
         pn_cells_l_sm,
         alexandrium_cells_l,
         dinophysis_cells_l,
         akashiwo_sanguinea_cells_l,
         dominant_spp,
         notes,
         # Cell percentages
         pn_perc,
         pn_perc_pm,
         pn_perc_afh,
         pn_perc_pdc,
         pn_perc_sm,
         pn_perc_lg,
         pda_ng_l,
         # Weather
         weather, swell_ft, wind_kts, tide_hi, tide_lo,
         # Everything
         everything())

# Inspect data
str(data)
freeR::complete(data)

# Hopefully perfect values
table(data$site)
range(data$date)

# Imperfect values
sort(unique(data$time))
sort(unique(data$swell_ft))
sort(unique(data$wind_kts))
sort(unique(data$tide_lo))
sort(unique(data$tide_hi))


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "ORHAB_2000_2020_beach_sampling_data.Rds"))
write.csv(data, file=file.path(outdir, "ORHAB_2000_2020_beach_sampling_data.csv"), row.names = F)

