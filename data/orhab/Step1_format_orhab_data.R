
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
files2merge <- list.files(file.path(indir, "ORHAB2000_manually_formatted"))[1:17]

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
      data_sheet <- readxl::read_excel(file.path(indir, "ORHAB2000_manually_formatted", x), sheet=y) %>%
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

# Format data
data <- data_orig %>%
  # Format date
  mutate(date1=ymd(date)) %>%
  # Arrange
  select(filename, sheet, site, date, date1, time, everything())

# Inspect data
str(data)
freeR::complete(data)

# Inspect values
sort(unique(data$site))

# Check
check <- data %>%
  filter(is.na(site))



# Export data
################################################################################










