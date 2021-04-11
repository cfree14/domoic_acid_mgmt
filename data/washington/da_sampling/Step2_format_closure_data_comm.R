

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/da_sampling/raw/Files for Chris Free"
outdir <- "data/washington/da_sampling/processed"
plotdir <- "data/washington/da_sampling/figures/closures/temp"
gisdir <- "data/washington/gis_data/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Shellview_CloseLog_2014_2020.xls.xlsx"))
freeR::complete(data_orig)

# Read zone key
zone_key <- readxl::read_excel(file.path(gisdir, "WDFW_closure_zone_key.xlsx")) %>%
  mutate(zone_id=as.character(zone_id))

# Read CGA key
cga_key <- read.csv(file.path(gisdir, "WDFW_commercial_growing_areas_key.csv"), as.is=T) %>%
  mutate(zone_type="Commercial growing area",
         growing_area_id=paste0("CGA-", growing_area_id)) %>%
  rename(zone=growing_area,
         zone_id3=growing_area_id)

# Read waterbody-to-zone and county-to-zone keys
county_zone_key_orig <-readxl::read_excel(file.path(outdir, "county_zone_key.xlsx"))
waterbody_zone_key_orig <- readxl::read_excel(file.path(outdir, "waterbody_zone_key.xlsx"))

# Split waterbodies/counties into zones
# Every area should have a basin
# Every area should have a zone id


# Format zone keys
################################################################################

# County key
county_zone_key <- county_zone_key_orig %>%
  janitor::clean_names("snake") %>%
  mutate(zone_type_orig="County") %>%
  rename(zone=county, zone_id2=closure_zones) %>%
  select(zone_type_orig, zone, zone_id2)

# County key
waterbody_zone_key <- waterbody_zone_key_orig %>%
  janitor::clean_names("snake") %>%
  mutate(closure_zones="78.01") %>%
  mutate(zone_type_orig="Waterbody") %>%
  rename(zone=waterbody_name, zone_id2=closure_zones) %>%
  select(zone_type_orig, zone, zone_id2)

# Merged key
wc_zone_key <- bind_rows(county_zone_key, waterbody_zone_key)

# Confirm that all zones are in zone key
zones <- paste(wc_zone_key$zone_id2, collapse = ", ") %>%
  strsplit(., ", ") %>% unlist(.) %>% unique(.) %>% sort(.)
zones[!zones%in%zone_key$zone_id] # yes all in there


# Step 1. Format data
################################################################################

# Composite species codes
all_species <- "Crabs, Razor Clams, Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"
all_species_but_crab <- "Razor Clams, Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"
all_species_but_crab_rclam <- "Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"

# Format data
data <- data_orig %>%
  # Rename columns
  janitor::clean_names("snake") %>%
  rename(zone_type_orig=parent_entity,
         zone_orig=parent_entity_name,
         date1=status_begin_date,
         date2=status_end_date,
         action=event_type,
         reason=closure_reason,
         reason_desc=reason_descr,
         species_orig=closed_for_species,
         geoduck_tract=geoduck_tract_number,
         comments=comment) %>%
  # Convert dates
  mutate(date1=date1 %>% substr(.,1, 10) %>% ymd(),
         date2=date2 %>% substr(.,1, 10) %>% ymd(),
         year=year(date1)) %>%
  # Format species
  mutate(species_orig=species_orig %>% gsub(",", ", ", .),
         species_code=recode(species_orig,
                             "All Crab Species"="CRAB",
                             "All Species"="ALL",
                             "All Species excluding Razor Clams"="ALL-BUT-RCLAM",
                             "All Species including Crab"="ALL",
                             "Butter Clams"="BCLAM",
                             "Butter Clams, Varnish Clams"="BCLAM-VCLAM",
                             "Geoducks"="GDUCK",
                             "Littleneck Clams"="LCLAM",
                             "Littleneck Clams, Manila Clams"="LCLAM-MCLAM",
                             "Manila Clams"="MCLAM",
                             "Manila Clams, Cockles"="MCLAM-COCKL",
                             "Mussels"="MUSS",
                             "Mussels, Oysters"="MUSS-OST",
                             "Oysters"="OYST",
                             "Razor Clams"="RCLAM",
                             "Scallop"="SCALL",
                             "Varnish Clams"="VCLAM"),
         species=recode(species_orig,
                        "Scallop"="Scallops",
                        "All Crab Species"="Crabs",
                        "All Species"=all_species_but_crab,
                        "All Species including Crab"=all_species,
                        "All Species excluding Razor Clams"=all_species_but_crab_rclam)) %>%
  # Add fishery column
  mutate(fishery=recode(action,
                        "Recreational Closure"="Recreational",
                        "Commercial Closure"="Commercial"),
         fishery_code=recode(fishery,
                             "Recreational"="REC",
                             "Commercial"="COMM",
                             "Advisory"="ADV")) %>%
  # All commercial growing area closure must be for commercial species
  mutate(fishery=ifelse(zone_type_orig=="Commercial Growing Area", "Commercial", fishery),
         fishery_code=ifelse(zone_type_orig=="Commercial Growing Area", "COMM", fishery_code)) %>%
  # Format zone type
  mutate(zone_type_orig=recode(zone_type_orig, "Water Body"="Waterbody"),
         zone_type=recode(zone_type_orig,
                          "County"="Closure Zone",
                          "Waterbody"="Closure Zone"),
         zone_type_orig=stringr::str_to_sentence(zone_type_orig),
         zone_type=stringr::str_to_sentence(zone_type)) %>%
  # Format zones
  mutate(zone_id=sub("\\-.*", "", zone_orig) %>% stringr::str_trim(),
         zone_id=ifelse(!grepl("-", zone_orig), NA, zone_id),
         zone=sub('.+-(.+)', '\\1', zone_orig) %>% stringr::str_trim() %>% stringr::str_to_title()) %>%
  # Add zone ids for waterbodys/countys
  left_join(wc_zone_key, by=c("zone_type_orig", "zone")) %>%
  mutate(zone_id=ifelse(is.na(zone_id), zone_id2, zone_id)) %>%
  select(-zone_id2) %>%
  # Add zone ids for CGAs
  left_join(cga_key %>% select(zone_type, zone, zone_id3), by=c("zone_type", "zone")) %>%
  mutate(zone_id=ifelse(is.na(zone_id), zone_id3, zone_id)) %>%
  select(-zone_id3) %>%
  # Format geoduct tract
  mutate(geoduck_tract=gsub("/ ", "/", geoduck_tract) %>% stringr::str_to_title()) %>%
  # Add closure zone basin
  # left_join(zone_key %>% select(zone_id, basin), by="zone_id") %>%
  # Add event id
  mutate(zone_for_event_id=ifelse(!is.na(zone_id) & nchar(zone_id)<10, zone_id, zone),
         event_id_orig=paste(date1, fishery_code, species_code, zone_for_event_id, sep="-") %>% make.unique()) %>%
  select(-zone_for_event_id) %>%
  # Remove empty columns -- CHECK WITH DOH
  select(-c(parcel_number, other_closed_species)) %>%
  # Remove action b/c it doesn't really contain information (all in fishery now)
  select(-action) %>%
  # Arrange
  select(event_id_orig, year, date1, date2,
         zone_type_orig, zone_type, zone_orig, zone_id, zone, geoduck_tract,
         species_orig, species_code, species,
         fishery_code, fishery, status, reason, reason_desc, comments,
         everything())

# Are event ids unique?
freeR::which_duplicated(data$event_id_orig)

# Inspect data
str(data)
freeR::complete(data)

# Inspect data
table(data$zone_type_orig)
table(data$zone_type)
table(data$zone)
table(data$geoduck_tract)
table(data$species)
table(data$species_code)
sort(unique(data$species_orig))
table(data$fishery)
table(data$status)
table(data$reason)

# Do zone type and fishery match ups make sense?
data %>%
  group_by(zone_type_orig, fishery) %>%
  summarize(n=n())

# Inspect zone key
zone_key_obs <- data %>%
  select(zone_type_orig, zone_orig, zone, zone_id, ) %>%
  unique() %>%
  arrange(zone_type_orig, zone)

# Zone type key
spp_zone_key <- data %>%
  group_by(species_orig, zone_type_orig) %>%
  summarize(nevents=n())

# Waterbodies
waterbodies <- data %>%
  filter(zone_type_orig=="Waterbody") %>%
  select(zone) %>%
  unique() %>%
  arrange(zone)

# Counties
counties <- data %>%
  filter(zone_type_orig=="County") %>%
  select(zone) %>%
  unique() %>%
  arrange(zone)


# Step 2. Split multi-zone rows
################################################################################

# Build basin key
basins_cz <- zone_key %>%
  select(zone_id, zone, basin)
basins_cga <- cga_key %>%
  select(zone_id3, zone, basin) %>%
  rename(zone_id=zone_id3)
basin_key <- bind_rows(basins_cz, basins_cga)

# One row per zone
data2 <- purrr::map_df(1:nrow(data), function(x) {

  # Get row
  row <- data %>% slice(x)

  # Identify zones represented in row
  zones_in_row_string <- row %>% pull(zone_id)
  zones_in_row_list <- strsplit(zones_in_row_string, split=", ")
  zones_in_row_cvec <- unlist(zones_in_row_list)

  # Duplicate row, if necessary
  nzones <- length(zones_in_row_cvec)
  if(nzones>1){
    row_out <- row %>%
      slice(rep(1:n(), each=nzones)) %>%
      mutate(zone_id=zones_in_row_cvec)
  }else{
    row_out <- row
  }

})

# Format
data2a <- data2 %>%
  # Add basin and zone name
  select(-zone) %>%
  left_join(basin_key, by="zone_id") %>%
  # Fill missing zones/basins
  mutate(zone=ifelse(zone_orig=="Lopez Sound", "Lopez Sound", zone),
         basin=ifelse(zone_orig=="Lopez Sound", "San Juan Islands and Strait of Georgia", basin),
         zone=ifelse(zone_orig=="200103 - SEMIAHMOO MARINA", "Semiahmoo Marina", zone),
         basin=ifelse(zone_orig=="200103 - SEMIAHMOO MARINA", "San Juan Islands and Strait of Georgia", basin)) %>%
  # Arrange
  select(event_id_orig, year, date1, date2,
         basin, zone_type_orig, zone_type, zone_orig, zone_id, zone, geoduck_tract,
         species_orig, species_code, species,
         fishery_code, fishery, status, reason, reason_desc, comments,
         everything())

# Inspect
freeR::complete(data2a)


# Step 3. Split multi-species rows
################################################################################

# One row per species
data3 <- purrr::map_df(1:nrow(data2a), function(x) {

  # Get row
  row <- data2a %>% slice(x)

  # Identify species represented in row
  species_in_row_string <- row %>% pull(species)
  species_in_row_list <- strsplit(species_in_row_string, split=", ")
  species_in_row_cvec <- unlist(species_in_row_list)

  # Duplicate row, if necessary
  nspecies <- length(species_in_row_cvec)
  if(nspecies>1){
    row_out <- row %>%
      slice(rep(1:n(), each=nspecies)) %>%
      mutate(species=species_in_row_cvec)
  }else{
    row_out <- row
  }

})

# Inspect
freeR::complete(data3)


# Step 4. Final formatting
################################################################################

# Final format
data4 <- data3 %>%
  # Format species
  mutate(species=stringr::str_to_sentence(species),
         species=gsub("clams", "clam", species),
         species=recode(species, "Cockles"="Cockle", "Crabs"="Crab",
                        "Geoducks"="Geoduck", "Scallops"="Scallop",
                        "Mussels"="Mussel", "Oysters"="Oyster")) %>%
  # Format species code
 mutate(species_code=recode(species,
                            "Butter clam" ="BCLAM",
                            "Cockle"="COCKL",
                            "Crab"="CRAB",
                            "Geoduck"="GDUCK",
                            "Littleneck clam"="LCLAM",
                            "Manila clam"="MCLAM",
                            "Mussel"="MUSS",
                            "Oyster"="OYST",
                            "Razor clam"="RCLAM",
                            "Scallop"="SCALL",
                            "Varnish clam"="VCLAM")) %>%
   # Format event id
   mutate(event_id=paste(date1, fishery_code, species_code, zone_id, sep="-") %>% make.unique()) %>%
  # Arrange
  select(event_id, event_id_orig, year, date1, date2,
         basin, zone_type_orig, zone_type, zone_orig, zone_id, zone, geoduck_tract,
         species_orig, species_code, species,
         fishery_code, fishery, status, reason, reason_desc, comments,
         everything())

# Inspect
freeR::complete(data4)

# Are ids unique?
freeR::which_duplicated(data4$event_id)

# Visualize
################################################################################

# Species
species <- sort(unique(data4$species))

# Loop through species
i <- 1
for(i in 1:length(species)){

  # Species
  spp_do <- species[i]
  print(paste(i, spp_do))

  # Data lines
  data_lines <- data4 %>%
    # Reduce
    filter(species==spp_do & zone_type=="Closure zone") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, date1, date2) %>%
    # Reduce to complete lines
    filter(!is.na(date1) & !is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 8:9) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open"))

  # Data points
  data_points <- data4 %>%
    # Reduce
    filter(species==spp_do & zone_type=="Closure zone") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, date1, date2) %>%
    # Reduce to complete lines
    filter(is.na(date1) | is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 8:9) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open")) %>%
    # Remove missing
    filter(!is.na(date))

  # Plot simple data
  g <- ggplot(data_lines, aes(x=date, y=zone, color=fishery, group=event_id)) +
    # facet_grid(basin~., scales="free", space="free") +
    ggforce::facet_col(vars(basin), scales = "free_y", space = "free") +
    geom_line() +
    # Start/end points
    geom_point(data=data_points, aes(x=date, y=zone, color=fishery, shape=date_type), inherit.aes = F) +
    # Labs
    labs(x="", y="", title=paste(spp_do, "closures")) +
    scale_color_discrete(name="Fishery") +
    scale_shape_manual(name="Decision", values=c(16, 21)) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=5),
          axis.title=element_blank(),
          legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          strip.text=element_text(size=6),
          plot.title=element_text(size=8),
          # Gridlines
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  g

  # Export
  outfile <- paste0("FigX_closures_", tolower(spp_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, outfile),
         width=6.5, height=9, units="in", dpi=600)

}




# Create time series
################################################################################

# Create a data frame like the following:
# species, fishery, year, month, date, zone_type, zone_id, zone_name, status

# Export
################################################################################

# Export
saveRDS(data4, file=file.path(outdir, "WA_DOH_2014_2020_biotoxin_closures.Rds"))
