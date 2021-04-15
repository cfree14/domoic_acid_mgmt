

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

# Read data
data_orig <- readRDS(file.path(outdir, "WA_DOH_2002_2020_biotoxin_closures_rec_w_sample_data.Rds"))

# Read zone key
zone_sf <- readRDS(file.path(gisdir, "WDFW_biotoxin_closure_zones.Rds"))
zone_key <- zone_sf %>%
  sf::st_drop_geometry() %>%
  mutate(zone_id=recode(zone_id, "20.1"="20.10")) # MAKE THIS A FIX IN ORIGIMAL
freeR::which_duplicated(zone_key$zone_id)


# Step 1. Reduce to open/closure events
################################################################################

# Reduce to events
data1 <- data_orig %>%
  # Identify events
  group_by(action_date, action_area, action_orig, action, action_species) %>%
  summarize(nsamples=n(),
            reason=paste(sort(unique(toxin)), collapse=", ")) %>%
  ungroup() %>%
  # Remove events w/out required info
  filter(!is.na(action_date) & !is.na(action_area) & !is.na(action) & !is.na(action_species) & action_species!="") %>%
  # Rename columns
  rename(date=action_date, area_orig=action_area, species_orig=action_species) %>%
  # Recode species
  mutate(species=recode(species_orig,
                         "All crab species"="Crab",
                         "All crab species and razor clams"="Crab, razor clam",
                         "All species"="Razor clam, butter clam, varnish clam, geoduck",
                         "All species except butter clams"="Razor clam, varnish clam, geoduck",
                         "All species except butter/varnish clams"="Razor clam, geoduck",
                         "All species except butter/varnish clams and geoducks"="Razor clam",
                         "All species except geoducks"="Razor clam, butter clam, varnish clam, geoduck",
                         "All species except geoducks and butter clams"="Razor clam, varnish clam",
                         "All species except varnish clams"="Razor clam, butter clam, geoduck",
                         "All species excluding razor clams"="Butter clam, varnish clam, geoduck",
                         "All species including crab"="Crab, razor clam, butter clam, varnish clam, geoduck",
                         "Butter/varnish clams"="Butter clam, varnish clam",
                         "Butter clams"="Butter clam",
                         "Razor clams"="Razor clam"))

# Inspect species
sort(unique(data1$species))


# Step 2. Break into constituent species
################################################################################


# One row per species
data2 <- purrr::map_df(1:nrow(data1), function(x) {

  # Get row
  row <- data1 %>% slice(x)

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

# Format data
data2a <- data2 %>%
  # Format species
  mutate(species=stringr::str_to_sentence(species)) %>%
  # Arrange
  select(date:species_orig, species)


# Step 3a. Build area key
################################################################################

# Build key
area_key_raw <- data_orig %>%
  # Unique areas
  group_by(action_area) %>%
  summarize(counties=paste(sort(unique(sample_county)), collapse=", ")) %>%
  ungroup() %>%
  # Add zone ids
  left_join(zone_key %>% select(zone_id, zone), by=c("action_area"="zone")) %>%
  mutate(type=ifelse(!is.na(zone_id), "zone", "")) %>%
  # Rename/add
  rename(area_orig=action_area, zone_ids=zone_id) %>%
  mutate(area="") %>%
  # Arrange
  select(area_orig, area, counties, type, zone_ids) %>%
  arrange(counties) %>%
  # Remove
  filter(!is.na(area_orig))

# Export key
# write.csv(area_key_raw, file=file.path(outdir, "WA_DOC_rec_closure_area_key.csv"), row.names=F)


# Step 3b. Add area names and zone ids
################################################################################

# Read finalized key
area_key <- readxl::read_excel(file.path(outdir, "WA_DOC_rec_closure_area_key.xlsx")) %>%
  mutate(zone_ids=recode(zone_ids,
                         "34.020000000000003"="34.02",
                         "64.010000000000005"="64.01",
                         "16.010000000000002"="16.01",
                         "2.0099999999999998"="2.01",
                         "78.010000000000005"="78.01"))

# Inspect key zones
area_key_zones <- area_key %>%
  pull(zone_ids) %>%
  paste(., collapse=", ") %>%
  strsplit(", ") %>%
  unlist() %>%
  unique() %>% sort()

# Are they all in the zone key?
area_key_zones[!area_key_zones %in% zone_key$zone_id]

# Add area key to data
data3 <- data2a %>%
  # Add area/zones
  left_join(area_key %>% select(area_orig, area, zone_ids), by=c("area_orig")) %>%
  # Arrange
  select(date, area_orig, area, zone_ids, everything())


# Step 4. Break apart into constituent zones
################################################################################

# One row per species
data4 <- purrr::map_df(1:nrow(data3), function(x) {

  # Get row
  row <- data3 %>% slice(x)

  # Identify values represented in row
  vals_in_row_string <- row %>% pull(zone_ids)
  vals_in_row_list <- strsplit(vals_in_row_string, split=", ")
  vals_in_row_cvec <- unlist(vals_in_row_list)

  # Duplicate row, if necessary
  nvals <- length(vals_in_row_cvec)
  if(nvals>1){
    row_out <- row %>%
      slice(rep(1:n(), each=nvals)) %>%
      mutate(zone_id=vals_in_row_cvec)
  }else{
    row_out <- row
  }

})

# Step 5. Format values
################################################################################

# Format data
data5 <- data4 %>%
  # Add basin
  left_join(zone_key %>% select(zone_id, basin, county), by="zone_id") %>%
  # Arrange
  select(date, basin, county, area_orig:zone_ids, zone_id, everything())



# Step 4. Quick visualizations of performance
################################################################################

# Species
spp <- sort(unique(data5$species))

# Loop through species
for(i in 1:length(spp)){

  # Subset data
  sdata <- data5 %>%
    filter(species==spp[i] & !is.na(zone_id))

  # Plot data
  g <- ggplot(sdata, aes(x=date, y=zone_id, shape=action, color=action)) +
    facet_grid(basin~., space="free", scales="free_y", labeller = label_wrap_gen(width=12)) +
    geom_point() +
    # Labels
    labs(x="Date", y="Zone", title=spp[i]) +
    scale_x_date(breaks=seq(ymd("2002-01-01"), ymd("2021-01-01"), by="1 year"), labels=2002:2021) +
    # Theme
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text=element_text(size=6),
          axis.title=element_text(size=8),
          legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          strip.text=element_text(size=8),
          plot.title=element_text(size=10),
          # Gridlines
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          # Legend
          legend.position="bottom")
  g

}



# Step 5. Export
################################################################################




