

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
  mutate(da_ppm=recode(da_ppm,
                      "< 2.5"="0",
                      "<2.5"="0",
                      "ND"="0",
                      "nd"="0"),
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
  # Arrange
  select(sample_id,
         year, date,
         comm_name, species, sex,
         port, area, block_id, block_lat_dd, block_long_dd, latlong_orig, fathoms,
         da_ppm, everything())

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

# Build lat/long key
latlong_key <- data %>%
  select(latlong_orig) %>%
  unique() %>%
  filter(!is.na(latlong_orig)) %>%
  mutate(nchar=nchar(latlong_orig)) %>%
  # Recode long ones into shorter ones
  mutate(latlong=recode(latlong_orig,
                        "N 33 59.598’ W 119 33.772’ or N 33 59.624’ W 119 34.298’"="33 59.598 -119 33.772",
                        "N 33 59.598’ W 119 33.772’ or N 33 59.624’ W 119 34.298’" = "33 59.598 -119 33.772",
                        "34 02.26 -119 31.50 or 34 02.45 -119 31.73" = "34 02.26 -119 31.50",
                        "1) 34 02.56, 119 31.93. 2) 34 02.69, 119 32.54. 3) 34 01.77, 119 36.52" = "34 02.56 -119 31.93",
                        "39 00.42 -123 44.772 and   39 01.25 -123 46.779" = "39 00.42 -123 44.772",
                        "N 40° 39.03’ W 124° 24.01’ or N 40° 38.95’ W 124° 24.05’" = "40 39.03 -124 24.01")) %>%
  # Recode stubborn ones
  mutate(latlong=ifelse(grepl("39 00.42", latlong), "39 00.42 -123 44.772", latlong),
         latlong=ifelse(grepl("N 40° 39.06", latlong), "40 39.03 -124 24.01", latlong)) %>%
  # Remove N/W
  mutate(latlong=gsub(" W|W| N|N", "", latlong)) %>%
  # Add negative signs and use negative to split
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
  # Split
  tidyr::separate(latlong, sep="-", remove=F, into=c("lat", "long")) %>%
  filter(is.na(long))



# Export
write.csv(latlong_key, file=file.path(datadir, "latlong_key.csv"), fileEncoding="UTF-8", row.names=F)




# Export data
################################################################################

