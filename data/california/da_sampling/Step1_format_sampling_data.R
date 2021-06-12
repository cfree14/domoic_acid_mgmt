

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "data/california/da_sampling/data"
plotdir <- "data/california/da_sampling/figures"

# Blocks
blocks <- wcfish::blocks %>%
  sf::st_drop_geometry()

# Read and merge data
################################################################################

# Read data
crab1 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Crab 2015-2017") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

crab2 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Crab 2017-2018") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

crab3 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Crab 2018-2019") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

crab4 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Crab 2019-2020") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

crab5 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Crab 2020-2021") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

lobster1 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Lobster 2016") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id",  "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

lobster2 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Lobster 2017-2018") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id", "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

lobster3 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Lobster 2018-2019") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id",  "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

lobster4 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Lobster 2019-2020") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id",  "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

lobster5 <- readxl::read_excel(file.path(datadir, "Free Request 06-09-2021 2015-2021 DA.xlsx"), sheet="Lobster 2020-2021") %>%
  setNames(c("sample_id", "port", "date", "comm_name", "area", "block_id",  "latlong", "fathoms", "da_ppm")) %>%
  mutate_all(as.character)

# Merge data
data_orig <- bind_rows(crab1, crab2, crab3, crab4, crab5,
                       lobster1, lobster2, lobster3, lobster4, lobster5)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Remove duplicated rows
  unique() %>%
  # Format date
  mutate(date=ymd(date),
         year=year(date)) %>%
  # Format DA concentration
  mutate(da_prefix=ifelse(grepl("<|ND|nd", da_ppm), "<", ""),
         da_ppm=recode(da_ppm,
                      "< 2.5"="2.5",
                      "<2.5"="2.5",
                      "ND"="2.5",
                      "nd"="2.5"),
         da_ppm=as.numeric(da_ppm)) %>%
  # Format port
  mutate(port=recode(port,
                     "Cresecent City"="Crescent City",
                     "Ft. Bragg"="Fort Bragg",
                     "Trindad"="Trinidad")) %>%
  # Format area
  mutate(area=recode(area,
                     "Pt. Arena"="Point Arena",
                     "Pt. Diablo"="Point Diablo",
                     "Santa Rosa -Channel Islands"="Santa Rosa Island",
                     "Santa Rosa- Channel Islands"="Santa Rosa Island")) %>%
  # Format block
  mutate(block_id=recode(block_id, "108/115"="115"),
         block_id=as.numeric(block_id)) %>%
  # Format sex
  mutate(sex=ifelse(comm_name=="Lobster (F)", "female",
                    ifelse(comm_name=="Lobster (M)", "male", "unknown"))) %>%
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "Spiny lobster"="California spiny lobster",
                          "Spiny lobsters"="California spiny lobster",
                          "Lobster"="California spiny lobster",
                          "Lobster (f)"="California spiny lobster",
                          "Lobster (m)"="California spiny lobster",
                          "Spider crab"="Sheep crab")) %>%
  # Add species
  mutate(species=recode(comm_name,
                        "Dungeness crab"="Metacarcinus magister",
                        "Rock crab"="Cancer spp.",
                        "Sheep crab"="Loxorhynchus grandis",
                        "Spiny lobster"="Panulirus interruptus")) %>%
  # Add block meta-data
  left_join(blocks %>% select(block_id, block_lat_dd, block_long_dd), by="block_id") %>%
  # Format lat/long
  rename(latlong_orig=latlong) %>%
  mutate(latlong_orig=stringr::str_trim(latlong_orig) %>% trimws()) %>%
  # Add survey id
  mutate(survey_id=paste(date, area, comm_name, sep="-")) %>%
  # Arrange
  select(survey_id, sample_id,
         year, date,
         comm_name, species, sex,
         port, area, block_id, block_lat_dd, block_long_dd, latlong_orig, fathoms,
         da_prefix, da_ppm, everything())

# Any duplicates?
anyDuplicated(data$sample_id)
freeR::which_duplicated(data$sample_id)

# Inspect
str(data)
freeR::complete(data)
range(data$date) # 1900???
table(data$year)
range(data$da_ppm)
table(data$port)
table(data$area)
table(data$sex)
table(data$comm_name)
table(data$species)
table(data$fathoms) # don't worry about this



# Format lat/longs
################################################################################

# Function to convert DMS (XX XX.XXXX) to DD (XX.XXXXXXXX)
conv_dd <- function(dms, type){
  if(type=="lat"){
    dd <- as.numeric(substr(dms, 1, 2)) + as.numeric(substr(dms, 4, nchar(dms)))/60
  }
  if(type=="long"){
    dd <- as.numeric(substr(dms, 1, 3)) + as.numeric(substr(dms, 5, nchar(dms)))/60
  }
  return(dd)
}


# Build lat/long key
latlong_key <- data %>%
  # Reduce to unique lat/longs
  select(latlong_orig) %>%
  unique() %>%
  filter(!is.na(latlong_orig)) %>%
  # Recode long ones into shorter ones
  mutate(nchar=nchar(latlong_orig)) %>%
  mutate(latlong=recode(latlong_orig,
                        "N 33 59.598’ W 119 33.772’ or N 33 59.624’ W 119 34.298’"="33 59.598 -119 33.772",
                        "N 33 59.598’ W 119 33.772’ or N 33 59.624’ W 119 34.298’" = "33 59.598 -119 33.772",
                        "34 02.26 -119 31.50 or 34 02.45 -119 31.73" = "34 02.26 -119 31.50",
                        "1) 34 02.56, 119 31.93. 2) 34 02.69, 119 32.54. 3) 34 01.77, 119 36.52" = "34 02.56 -119 31.93",
                        "39 00.42 -123 44.772 and   39 01.25 -123 46.779" = "39 00.42 -123 44.772",
                        "N 40° 39.03’ W 124° 24.01’ or N 40° 38.95’ W 124° 24.05’" = "40 39.03 -124 24.01")) %>%
  # Recode stubborn long ones into shorter ones (they have weird characters hidden in them)
  mutate(latlong=ifelse(grepl("39 00.42", latlong), "39 00.42 -123 44.772", latlong),
         latlong=ifelse(grepl("N 40° 39.06", latlong), "40 39.03 -124 24.01", latlong)) %>%
  # Remove N/W
  mutate(latlong=gsub(" W|W| N|N", "", latlong)) %>%
  # Add negative signs and use the negative sign to separate into seperate columns
  mutate(latlong=gsub("- ", "-", latlong),
         latlong=gsub(" / 123", " -123", latlong),
         latlong=gsub(" 124.19.13", " -124.19.13", latlong),
         latlong=gsub(", 117", " -117", latlong),
         latlong=gsub(", 119", " -119", latlong),
         latlong=gsub(" 119", " -119", latlong),
         latlong=gsub(", 120", " -120", latlong),
         latlong=gsub("' 120", " -120", latlong),
         latlong=gsub(", 122", " -122", latlong),
         latlong=gsub(", 123", " -123", latlong),
         latlong=gsub(" 123", " -123", latlong),
         latlong=gsub(" 124", " -124", latlong)) %>%
  tidyr::separate(latlong, sep="-", remove=F, into=c("lat", "long")) %>%
  # Remove bells and whistles
  mutate(lat=gsub(",|'|°|’", "", lat) %>% stringr::str_trim(),
         long=gsub(",|'|°|’", "", long)%>% stringr::str_trim()) %>%
  # Remove double spaces
  mutate(lat=gsub("  ", " ", lat),
         long=gsub("  ", " ", long)) %>%
  # Simplify "Unknown" values
  mutate(lat=ifelse(lat %in% c("Doran Beach", "not avail / SB Operation", "not available"), NA, lat)) %>%
  # Mark as DMS or DD
  mutate(type=ifelse(grepl(" ", lat), "DMS", "DD")) %>%
  # Fix lats with two periods
  mutate(lat=recode(lat,
                    "38.29.00"="38 29.00",
                    "38.32.50"="38 32.50",
                    "38.29.017"="38 29.017",
                    "38.32.701"="38 32.701",
                    "38.17.065"="38 17.065",
                    "38.15.432"="38 15.432",
                    "38.15.602"="38 15.602",
                    "37.19.33"="37 19.33",
                    "37.19.77"="37 19.77",
                    "37.30.67"="37 30.67",
                    "37.30.70"="37 30.70",
                    "37.50.58"="37 50.58",
                    "37.45.74"="37 45.74",
                    "37.49.32"="37 49.32",
                    "40.48.91"="40 48.91",
                    "40.49.72"="40 49.72",
                    "35.01.62"="35 01.62",
                    "35.01.53"="35 01.53",
                    "39.49.885"="39 49.885",
                    "39.49.687"="39 49.687",
                    "41.46.62"="41 46.62",
                    "39.48.499"="39 48.499",
                    "39.47.835"="39 47.835")) %>%
  mutate(long=recode(long,
                     "123.12.00"="123 12.00",
                     "123.18.01"="123 18.01",
                     "123.12.389"="123 12.389",
                     "123.18.751"="123 18.751",
                     "123.12.5"="123 12.5",
                     "123.18.8"="123 18.8",
                     "123.00.536"="123 00.536",
                     "123.03.047"="123 03.047",
                     "123.04.158"="123 04.158",
                     "122.29.07."="122 29.07",
                     "122.30.00."="122 30.00",
                     "122.38.47."="122 38.47",
                     "122.36.22."="122 36.22",
                     "123.40.04"="123 40.04",
                     "122.49.07"="122 49.07",
                     "122.54.92"="122 54.92",
                     "124.14.87"="124 14.87",
                     "124.15.84"="124 15.84",
                     "120.41.70"="120 41.70",
                     "120.46.84"="120 46.84",
                     "123.13.24"="123 13.24",
                     "123.53.597"="123 53.597",
                     "123.54.413"="123 54.413",
                     "124.19.13"="124 19.13",
                     "123.53.120"="123 53.120",
                     "123.53.124"="123 53.124")) %>%
  # Correct a few with two spaces
  mutate(lat=recode(lat,
                    "41 45 .760"="41 45.760",
                    "34 00 945"="34 00.945",
                    "37 33 29"="37.55806", # 37+33/60+29/3600
                    "37 33 29.5"="37.55819", # 37+33/60+29.5/3600
                    "37 18 30.7"="37.30853", # 37+18/60+30.7/3600
                    "37 17 17.1"="37.28808")) %>% # 37+17/60+17.1/3600
  mutate(long=recode(long,
                   "119 21 340"="119 21.340",
                   "122 25 56.0"="122.4322", # 122+25/60+56.0/3600
                   "122 32 01.6"="122.5338", # 122+32/60+01.6/3600
                   "122 34 29.2"="122.5748", # 122+34/60+29.2/3600
                   "122 42 40.1"="122.7111")) %>% # 122+42/60+40.1/3600
  # Recode a few I logicked to be errors (in Oregon and repeated)
  mutate(lat=recode(lat, "43 46.729"="41 46.729", "42 46.729"="41 46.729")) %>%
  # Mark as DMS or DD
  select(-type) %>%
  mutate(lat_type=ifelse(grepl(" ", lat), "DMS", "DD"),
         long_type=ifelse(grepl(" ", long), "DMS", "DD")) %>%
  # Convert to DD
  mutate(lat_dd=ifelse(lat_type=="DD", lat, conv_dd(lat, type="lat")),
         long_dd=ifelse(long_type=="DD", long, conv_dd(long, type="long"))) %>%
  # Convert to numeric
  mutate(lat_dd=as.numeric(lat_dd),
         long_dd=as.numeric(long_dd)) %>%
  # Make longs negative
  mutate(long_dd=-1*long_dd)


# Inspect
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
g <- ggplot() +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  geom_point(data=latlong_key, mapping=aes(x=long_dd, y=lat_dd)) +
  coord_sf(xlim=c(-125, -116), ylim=c(32, 44)) +
  theme_bw()
g


# Add lat/longs to data
################################################################################

# Build data
data2 <- data %>%
  # Add lat/longs
  left_join(latlong_key %>% select(latlong_orig, lat_dd, long_dd), by="latlong_orig") %>%
  rename(lat_dd_rep=lat_dd, long_dd_rep=long_dd) %>%
  # Select lat/longs
  mutate(lat_dd=ifelse(!is.na(lat_dd_rep), lat_dd_rep, block_lat_dd),
         long_dd=ifelse(!is.na(long_dd_rep), long_dd_rep, block_long_dd)) %>%
  # Arrange
  select(survey_id, sample_id,
         year, date,
         comm_name, species, sex,
         port, area, block_id, block_lat_dd, block_long_dd,
         latlong_orig, lat_dd_rep, long_dd_rep, lat_dd, long_dd, fathoms,
         da_prefix, da_ppm, everything())


# Are block and report lats related? WOW. Yes, very good
plot(lat_dd_rep ~ block_lat_dd, data2)
plot(long_dd_rep ~ block_long_dd, data2)

# Plot map
g <- ggplot() +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2) +
  geom_point(data=data2, mapping=aes(x=long_dd, y=lat_dd, color=port)) +
  coord_sf(xlim=c(-125, -116), ylim=c(32, 44)) +
  theme_bw()
g

freeR::complete(data2)

# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(datadir, "CDPH_2015_2021_crustacean_data.Rds"))

