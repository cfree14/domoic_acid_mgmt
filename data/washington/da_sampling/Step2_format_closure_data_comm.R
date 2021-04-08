

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
plotdir <- "data/washington/da_sampling/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Shellview_CloseLog_2014_2020.xls.xlsx"))
freeR::complete(data_orig)


# Format data
################################################################################

# Map zones
# Mark zones as ocean, bays, puget sound?

# Composite species codes
all_species <- "Crabs, Razor Clams, Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"
all_species_but_crab <- "Razor Clams, Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"
all_species_but_crab_rclam <- "Butter Clams, Geoducks, Littleneck Clams, Manila Clams, Cockles, Mussels, Oysters, Scallops, Varnish Clams"

# Format data
data <- data_orig %>%
  # Rename columns
  janitor::clean_names("snake") %>%
  rename(zone_type=parent_entity,
         zone=parent_entity_name,
         date1=status_begin_date,
         date2=status_end_date,
         action=event_type,
         reason=closure_reason,
         reason_desc=reason_descr,
         species_orig=closed_for_species,
         geoduck_tract=geoduck_tract_number) %>%
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
  # Add event id
  mutate(event_id=paste(date1, fishery_code, species_code, zone, sep="-")) %>%
  # Remove empty columns -- CHECK WITH DOH
  select(-c(parcel_number, other_closed_species)) %>%
  # Remove action b/c it doesn't really contain informtation (all in fishery now)
  select(-action) %>%
  # Format zones
  mutate(zone_id=sub("\\-.*", "", zone) %>% stringr::str_trim(),
         zone_id=ifelse(!grepl("-", zone), NA, zone_id),
         zone_name=sub('.+-(.+)', '\\1', zone) %>% stringr::str_trim()) %>%
  # Arrange
  select(event_id, year, date1, date2,
         zone_type, zone, zone_id, zone_name, geoduck_tract,
         species_orig, species_code, species,
         fishery_code, fishery, status, reason, reason_desc, comment,
         everything())

# Are event ids unique?
freeR::which_duplicated(data$event_id)

# Inspect data
str(data)
freeR::complete(data)

# Inspect data
table(data$zone_type)
table(data$zone)
table(data$geoduck_tract)
table(data$species)
table(data$species_code)
sort(unique(data$species_orig))
table(data$fishery)
table(data$status)
table(data$reason)

# Zone key
zone_key <- data %>%
  select(zone_type, zone_id, zone_name, zone) %>%
  unique() %>%
  arrange(zone_type, zone)


# Separate multi-species rows
################################################################################

# Step 2. One row per fishery
data2 <- purrr::map_df(1:nrow(data), function(x) {

  # Get row
  row <- data %>% slice(x)

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

# Create simple data
data2_simple <- data2 %>%
  # Simplify
  select(event_id, species, fishery, status, zone_type, zone, date1, date2) %>%
  # Gather
  gather(key="date_type", value="date", 7:8) %>%
  mutate(date_type=recode(date_type,
                          "date1"="Close", "date2"="Open"))

# Data to plot
data_plot <- data2_simple %>% filter(species=="Crabs")

# Plot simple data
g <- ggplot(data_plot, aes(x=date, y=zone, color=fishery, group=event_id)) +
  geom_line() +
  # Start/end points
  geom_point(data=data_plot, aes(x=date, y=zone, color=fishery, shape=date_type), inherit.aes = F) +
  # Labs
  labs(x="", y="") +
  scale_color_discrete(name="Fishery") +
  scale_shape_manual(name="Decision", values=c(16, 21)) +
  # Theme
  theme_bw()
g

# Create time series
################################################################################

# Create a data frame like the following:
# species, fishery, year, month, date, zone_type, zone_id, zone_name, status

# Visualize
################################################################################



# Export
################################################################################


