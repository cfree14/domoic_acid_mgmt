
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
site_key <- readxl::read_excel(file.path(outdir, "ORHAB_beach_sampling_sites.xlsx"))

# Plot sites
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
canada <- rnaturalearth::ne_states(country="Canada", returnclass = "sf")
g <- ggplot() +
  geom_sf(data=usa, color="white", fill="grey80") +
  geom_sf(data=canada, color="white", fill="grey80") +
  # Stations
  geom_point(data=site_key, mapping=aes(x=long_dd, y=lat_dd, color=coord_type)) +
  ggrepel::geom_text_repel(data=site_key, mapping=aes(x=long_dd, y=lat_dd, label=site), size=2) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="") +
  # Crop
  coord_sf(xlim=c(-125, -123), ylim=c(46.25, 48.5)) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position = c(0.2,0.1),
        legend.background = element_rect(fill=alpha('blue', 0)))
g

# Export
ggsave(g, filename=file.path(plotdir, "ORHAB_sampling_site_map.png"),
       width=8.5, height=11, units="in", dpi=600)


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
  # Format time
  rename(time_orig=time) %>%
  # a) Recome some
  mutate(time1=recode(time_orig,
                      "13:40 Pm"="13:40",
                      "8:55"="08:55",
                      "11;30"="11:30",
                      "13;55"="13:55",
                      "15;30"="15:30",
                      "10.3"="10:30",
                      "10.57"="10:57",
                      "13.4"="13:40",
                      "9.34"="09:34",
                      "900am"="09:00",
                      "230 PM"="14:30",
                      "1:30PM"="13:30",
                      "1:25: PM"="13:25",
                      "14:46 PM"="14:46",
                      "13:30 PM"="13:30",
                      "14:20 PM"="14:20",
                      "13:00 PM"="13:00",
                      "13:25 PM"="13:25",
                      "14:00 PM"="14:00",
                      "16:22 PM"="16:22",
                      "13:20 PM"="13:20",
                      "13:50 PM"="13:50",
                      "14:35 PM"="14:35",
                      "13:00 PM"="13:00",
                      "13:45 PM"="13:45",
                      "14:00 PM"="14:00",
                      "13:20 PM"="13:20",
                      "14:10 PM"="14:10",
                      "13:10 PM"="13:10",
                      "13:00 PM"="13:00",
                      "13:40 PM"="13:40",
                      "2:00PM"="14:00",
                      "1:30PM"="13:30",
                      "7:45pm"="19:45",
                      "4:45pm"="16:45",
                      "5:40pm"="17:40",
                      "6:20pm"="18:20",
                      "1:40pm"="13:40",
                      "2:15pm"="14:15",
                      "11:40AM"="11:40",
                      "8:15AM"="08:15",
                      "10:40AM"="10:40",
                      "6:30 AM"="06:30",
                      "10:00am"="10:00",
                      "10:00am"="10:00",
                      "10:05am"="10:05",
                      "10:40am"="10:40",
                      "9:37am"="09:37",
                      "6:00am"="06:00",
                      "9:35am"="09:35",
                      "5.00am"="05:00",
                      "7:48am"="07:48",
                      "9:15am"="09:15",
                      "8:05am"="08:05",
                      "9:10am"="09:10")) %>%
  # a) Pad to four letters
  mutate(time1=ifelse(nchar(time1)==3 & !grepl("\\.", time1), paste0("0", time1), time1)) %>%
  # b) Add colons
  mutate(time1=ifelse(nchar(time1)==4 & !grepl("\\.", time1),
                     paste0(substr(time1, 1, 2), ":", substr(time1, 3, 4)),
                     time1)) %>%
  # c) Erase long ones
  # mutate(time1=ifelse(nchar(time1)>5, NA, time1)) %>%
  # c) Convert to time
  # mutate(time2=hm(time1)) %>%
  # Format site to match site key
  rename(site_orig=site) %>%
  mutate(site_orig=stringr::str_trim(site_orig)) %>%
  mutate(site=recode(site_orig,
                     # La Push 1st beach
                     "La Push - First Beach"="La Push, First Beach",
                     "La Push First Beach"="La Push, First Beach",
                     "First Beach, La Push"="La Push, First Beach",
                     "La Push, First Beach"="La Push, First Beach",
                     "La Push, Beach 1" = "La Push, First Beach",

                     # La Push 2nd beach
                     "La Push - Second Beach"="La Push, Second Beach",
                     "La Push Second Beach"="La Push, Second Beach",
                     "La Push, Second Beach"="La Push, Second Beach",
                     "Second Beach, La Push"="La Push, Second Beach",

                     # Hobuck Beach
                     "Hobuck"="Hobuck Beach",
                     "Hobuck Beach, Makah Bay"="Hobuck Beach", # check
                     "Holbuck"="Hobuck Beach",
                     "Ho Beach"="Hobuck Beach",

                     # Quinault Beach
                     "Quilault Beach"="Quinault Beach",
                     "Quinault"="Quinault Beach",

                     # Westport Marina
                     "WES"="Westport",
                     "Westport WES"="Westport",
                     "Westport"="Westport",
                     "Westport Marina"="Westport",


                     # Tokeland
                     "TOK"="Tokeland",
                     "Toke Point"="Tokeland",
                     "Toke Point TOK"="Tokeland",
                     "Tokeland"="Tokeland",
                     "Tokeland Marina"="Tokeland",

                     # Long Beach
                     "LGB"="Long Beach",
                     "LB"="Long Beach",

                     # MocRocks
                     "MocRocks"="Mocrocks Beach",
                     "Mocrocks"="Mocrocks Beach",

                     # Willipa Bay
                     "Bay Center Mooring"="Willapa Bay mooring",
                     "Willapa Bay-Bay Center"="Bay Center",
                     "Willapa Bay"="Willapa Bay mooring",

                     # Others
                     "Neah Bay Marina"="Neah Bay",
                     "RaftRiver"="Raft River",
                     "THB"="Twin Harbors",
                     "Lone Tree Oyster LTO"="Lone Tree Oyster Company",
                     "Long island green maker 13"="Long Island 13",
                     "north Long Island Naselle channel"="North Long Island",
                     "Copalis"="Copalis Beach",
                     "Kalaloch"="Kalaloch Beach",
                     "Elk River"="Elk River Bridge")) %>%
  # Add lat/long
  select(-c(lat_dd, long_dd)) %>%
  left_join(site_key, by="site") %>%
  # Format depth
  mutate(depth_m=recode(depth_m, "surface"="0"),
         depth_m=as.numeric(depth_m)) %>%
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
  mutate(unknown_us_cm=as.numeric(unknown_us_cm),
         conductivity=as.numeric(conductivity),
         conductivity=ifelse(!is.na(conductivity), conductivity, unknown_us_cm)) %>%
  select(-unknown_us_cm) %>%
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
         pn_perc_pdc=as.numeric(pn_perc_pdc)) %>%
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
  select(-c(unknown1, unknown2, unknown3)) %>%
  select(-c(counter_inits, chl_inits, toxin_inits, sampler_inits, tower_inits)) %>%
  # Remove empty columns
  select(-c(pda_ng_l...30,  ...29)) %>%
  # Arrange
  select(filename, sheet, site, site_orig, site_id,
         # Date/time
         year, month, week, date,
         time_orig, time1,
         # Lat/long/depth
         coord_type, lat_dd, long_dd, depth_m,
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
sort(unique(data$time1))
sort(unique(data$swell_ft))
sort(unique(data$wind_kts))
sort(unique(data$tide_lo))
sort(unique(data$tide_hi))

# Inspect imperfect times
sort(unique(data$time1[nchar(data$time1)>5]))
sort(unique(data$time1[nchar(data$time1)<5]))
sort(unique(data$time1[nchar(data$time1)==5]))

# Build site key
site_key1 <- data %>%
  # Stats
  group_by(site, site_id, coord_type, lat_dd, long_dd) %>%
  summarize(n_obs=n(),
            sites=paste(sort(unique(site_orig)), collapse = "; ")) %>%
  ungroup() %>%
  # Arrange
  select(site, site_id, sites, coord_type, lat_dd, long_dd, n_obs) %>%
  arrange(coord_type, site_id)

# Export
write.csv(site_key1, file=file.path(outdir, "ORHAB_beach_sampling_sites_for_review.csv"), row.names = F)



# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "ORHAB_2000_2020_beach_sampling_data.Rds"))
write.csv(data, file=file.path(outdir, "ORHAB_2000_2020_beach_sampling_data.csv"), row.names = F)

