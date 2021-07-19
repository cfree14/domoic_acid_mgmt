

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/california/da_sampling/data/raw"
outdir <- "data/california/da_sampling/data"
plotdir <- "data/california/da_sampling/figures"

# Read data
data1_orig <- readxl::read_excel(file.path(indir, "CDPH_DA_2000-2013.xls.xlsx")) # 2000-2013 data
data2_orig <- readxl::read_excel(file.path(indir, "DA_2014-2021_CDPH_061121.xls.xlsx"), sheet=2) # 2014-2021 data
data3_orig <- readxl::read_excel(file.path(indir, "CDPH_DA_2000_crab.xlsx")) # 2000-2016 crab data
data_orig <- bind_rows(data1_orig, data2_orig, data3_orig)

# Read product key
prod_key <- readxl::read_excel(file.path(indir, "bivalve_product_key.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(sampleid=srl_number, date=date_sampled, site=sample_site,
         da_oper=mod_asp, da_ppm=asp_ug_g, lat_dd=latitude, long_dd=longitude, nindivs=number_of_individuals) %>%
  # Format date
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  # Add sample info
  left_join(prod_key, by="sample_type") %>%
  # Add scientific names
  mutate(species=recode(comm_name,
                        "Basket cockle"="Clinocardium nuttallii",
                        "Barred surfperch"="Amphistichus argenteus",
                        "Black surfperch"="Embiotoca jacksoni",
                        "Bay mussel"="Mytilus galloprovincialis", # Mediterranean mussel
                        "Butter clam"="Saxidomus giganteus",
                        "California yellowtail"="Seriola lalandi",
                        "Gaper clam"="Tresus nuttallii",
                        "Gooseneck barnacle"="Pollicipes polymerus",
                        "Grunion"="Leuresthes spp.",
                        "Kumamoto oyster"="Crassostrea sikamea",
                        "Littleneck clam"="Leukoma staminea", # Pacific littleneck clam
                        "Mackerel"="Scombridae spp.",
                        "Manila clam"="Venerupis philippinarum",
                        "Mussel spp."="Mytilus spp.",
                        "Northern anchovy"="Engraulis mordax",
                        "Pacific lamprey"="Entosphenus tridentatus",
                        "Pacific oyster"="Crassostrea gigas",
                        "Pile surfperch"="Rhacochilus vacca",
                        "Pismo clam"="Tivela stultorum",
                        "Razor clam"="Siliqua patula", # Pacific razor clam
                        "Red abalone"="Haliotis rufescens",
                        "Rock scallop"="Crassadoma gigantea",
                        "Salmon"="Oncorhynchus spp.",
                        "Sardine"="Sardinops sagax",
                        "Sea mussel"="Mytilus californianus",
                        "Sea otter"="Enhydra lutris",
                        "Shiner surfperch"="Cymatogaster aggregata",
                        "Spiny lobster"="Panulirus interruptus",
                        "Squid"="Doryteuthis opalescens",
                        "Thornback ray"="Raja clavata",
                        "Washington clam"="Saxidomus nuttalli",
                        "Brown rock crab"="Romaleon antennarius",
                        "Dungeness crab"="Metacarcinus magister",
                        "Pelagic red crab"="Pleuroncodes planipes",
                        "Red rock crab"="Cancer productus",
                        "Rock crab"="Rock crab spp.",
                        "Spider crab"="Loxorhynchus grandis",
                        "Stone crab"="Unknown",
                        "Swimming crab"="Portunus xantusii",
                        "Yellow rock crab"="Metacarcinus anthonyii")) %>%
  # Arrange
  select(sampleid,
         year, month, date,
         county, site, lat_dd, long_dd,
         sample_type, comm_name, species, type, tissue, nindivs, notes,
         da_oper, da_ppm, everything())


# Inspect
str(data)
freeR::complete(data)
table(data$da_oper)
table(data$county)
table(data$sample_type)
table(data$comm_name)
table(data$species) # Clam spp.
table(data$tissue)
table(data$type)

# Inspect species
spp_key <- data %>%
  select(sample_type, comm_name, species, tissue, type) %>%
  unique()
write.csv(spp_key, file.path(indir, "bivalve_product_key.csv"), row.names=F)

# Export data
saveRDS(data, file=file.path(outdir, "CDPH_2000_2021_other_data.Rds"))



