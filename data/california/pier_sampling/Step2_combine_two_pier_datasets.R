

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/california/pier_sampling/data/raw"
outdir <- "data/california/pier_sampling/data"

# Where does this data come from?
# CA HABMAP: https://calhabmap.org/
# SCCOOS ERDDAP server home page: https://erddap.sccoos.org/erddap/index.html
# SCCOOS ERDDAP server datasets: https://erddap.sccoos.org/erddap/info/index.html?page=1&itemsPerPage=1000
# Example meta-data

# Piers
# Bodega Bay (not on server), Trindad Pier (barely), Goleta Pier (in all file), Santa Cruz Wharf,
# Monterey Wharf, Cal Poly Pier, Stearns Wharf, Santa Monica Pier, Newport Pier, Scripps Pier

# Read data
data1_orig <- readRDS(file.path(outdir, "2005_2021_pier_sampling_data.Rds")) # each pier had its own file
data2_orig <- readRDS(file.path(outdir, "1969_2019_pier_sampling_data.Rds")) # all piers were in one file


# Format data
################################################################################

# Add sample id
data1 <- data1_orig %>%
  mutate(sample_id_me = paste(location, date, sep="-"),
         source="Merged individual files")
data2 <- data2_orig %>%
  mutate(sample_id_me = paste(location, date, sep="-"),
         source="Single master file")

# Identify observations from "all pier" file not in "individual pier" file
data2_add <- data2 %>%
  filter(!sample_id_me %in% data1$sample_id_me)

# Merge files
data <- bind_rows(data1, data2_add) %>%
  # Arrange
  select(-sample_id_me) %>%
  mutate(year=year(date)) %>%
  select(source, location:long_dd, year, everything()) %>%
  arrange(location, date_time)

# Add location
# Bodega Bay Marine Lab Intake: 38.316996,
location_add <- tibble(location="Bodega Bay",
                       lat_dd=38.316996,
                       long_dd=-123.070237,
                       nobs=NA,
                       nyears=NA,
                       years=NA)

# Location stats
locations <- data %>%
  # Stats
  group_by(location, lat_dd, long_dd) %>%
  summarize(nobs=n(),
            nyears=n_distinct(year),
            years=year %>% unique() %>% sort() %>% paste(., collapse=", ")) %>%
  # Recode years
  mutate(years=recode(years,
                      "1969, 1999, 2001, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021"="1969, 1999, 2001, 2008-2021",
                      "1969, 2000, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021"="1969, 2000, 2008-2021",
                      "1969, 2002, 2005, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020"="1969, 2002, 2005, 2010-2020",
                      "1969, 2001, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021"="1969, 2001, 2008-2021",
                      "2002, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021"="2002, 2011-2021",
                      "2005, 2009, 2010, 2011"="2005, 2009-2011",
                      "2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020"="2008-2020",
                      "2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021"="2008-2021")) %>%
  # Add Bodega Bay
  bind_rows(location_add) %>%
  # Add public?
  mutate(public=ifelse(location=="Bodega Bay", "Not public", "Public")) %>%
  # Arrange
  select(location, public, everything()) %>%
  arrange(desc(lat_dd))

# Export data
saveRDS(data, file=file.path(outdir, "1969_2021_pier_sampling_data_final.Rds"))
write.csv(locations, file=file.path(outdir, "CA_pier_sampling_locations.csv"), row.names = F)


