

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
outdir <- "data/oregon/processed"
plotdir <- "data/oregon/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "domoic.ODA.xlsx"))
clams_orig <- readxl::read_excel(file.path(indir, "clams.xlsx"))
mussels_orig <- readxl::read_excel(file.path(indir, "mussels.xlsx"))

# Read product key
product_key <- readxl::read_excel(file.path(outdir, "product_key.xlsx")) %>%
  rename(location2=location)

# Read site key
site_key <- readxl::read_excel(file.path(outdir, "OR_da_sampling_site_key.xlsx"))

# Questions for Alex Manderson
# Scientific names
# Figure out site locations
# Ask about "Crab Viscera- General History"

# Format clams/mussels data
################################################################################

# Format data
clams <- clams_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(sample_date=sampled_date, location=collection_site,
         da_oper=vari_for_da, da_ppm=domoic_acid, psp_oper=vari_for_psp, psp_ppm=psp_toxins) %>%
  mutate(species="Clams")

# Format data
mussels <- mussels_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(sample_date=sampled_date, location=collection_site,
         da_oper=vari_for_da, da_ppm=domoic_acid, psp_oper=vari_for_psp, psp_ppm=psp_toxins) %>%
  mutate(species="Mussels")

# Merge shellfish
shellfish <- bind_rows(clams, mussels) %>%
  # Format date
  mutate(sample_date=ymd(sample_date)) %>%
  # Format location
  rename(location_orig=location) %>%
  mutate(location=stringr::str_to_title(location_orig),
         location=recode(location,
                         # "Alsea Bay"
                         # "Bandon Beach"
                         "Bastendorf Bch To Cape Arago"="Bastendorf Beach to Cape Arago",
                         "Bay Ocean Spit  At Cape Meares"="Cape Meares-Bay Ocean Spit",
                         # "Bob Creek"="",
                         "Cannon Beach - Ecola St Park"="Cannon Beach-Ecola State Park",
                         "Cape Falcon @ Oswald West Stpk"="Cape Falcon-Oswald West State Park",
                         # "Cape Meares"="",
                         "Cape Perpetua Neptune Sp"="Cape Perpetua-Neptune State Park",
                         "Clatsop Bch-Cannon Beach"="Clatsop Beach-Cannon Beach",
                         "Clatsop Beach/Del Rey"="Clatsop Beach-Del Rey",
                         "Clatsop Beach/Gearhart"="Clatsop Beach-Gearhart",
                         "Clatsop Beach/Seaside"="Clatsop Beach-Seaside",
                         "Clatsop Beach/South Jetty"="Clatsop Beach-South Jetty",
                         "Clatsop Beach/Sunset"="Clatsop Beach-Sunset",
                         "Clatsop Beaches"="Clatsop Beach",
                         # "Clatsop County Area" ="",
                         "Coos Bay - Lower"="Coos Bay-Lower",
                         "Coos Bay - Upper"="Coos Bay-Upper",
                         "Coos N Jetty & Spit"="Coos Bay-North Jetty",
                         # "Food Safety Division"="",
                         "Gold Beach - Myers Creek"="Gold Beach-Myers Creek",
                         # "Harris Beach"="",
                         # "Lane County Area"="",
                         # "Lincoln County Area"="",
                         # "N Lincoln County"="",
                         # "Nehalem Bay"="",
                         "Nehalem Bay - Hanging"="Nehalem Bay-Hanging",
                         # "Netarts Bay"="",
                         "Netarts Bay\r"="Netarts Bay",
                         "Newport Agate Beach"="Newport Beach-Agate Beach",
                         "Newport Beaches"="Newport Beach",
                         "Newport Beaches @ Agate Beach"="Newport Beach-Agate Beach",
                         "Newport Beaches @ North Jetty"="Newport Beach-North Jetty",
                         "Newport Beaches @ South Beach"="Newport Beach-South Beach",
                         "Newport North Jetty"="Newport Beach-North Jetty",
                         "No Spit-Umpqua River"="Umpqua River-North Jetty",
                         # "Ona Beach State Park"="",
                         "Port Orford Hanging Site"="Port Orford-Hanging Site",
                         # "Seal Rock State Park"="",
                         # "Siletz Bay"="",
                         # "Silver Point"="",
                         # "Siuslaw Bay"="",
                         "So Jetty Columbia River"="Columbia River-South Jetty",
                         "So Jetty-Umpqua River"="Umpqua River-South Jetty",
                         "South Slough/Coos Bay" ="Coos Bay-South Slough",
                         # "Tillamook Bay"="",
                         "Tillamook Bay - Hanging"="Tillamook Bay-Hanging Site",
                         # "Tillamook County Area" ="",
                         "Waldport @ Beach"="Waldport-Beach",
                         "Whiskey Run/Mid Coos Co Bchs" ="Whiskey Run/Mid-Coos County Beaches"
                         # "Yachats River"="",
                         # "Yaquina Bay"=""
                         )) %>%
  # Arrange
  select(species, sample_date, location, everything()) %>%
  arrange(species, sample_date, location)

# Inspect data
str(shellfish)
freeR::complete(shellfish)
range(shellfish$sample_date)
table(shellfish$da_oper)
table(shellfish$psp_oper)
table(shellfish$location)

# Export
saveRDS(shellfish, file=file.path(outdir, "ODA_1999_2015_da_sampling_data_clams_mussels.Rds"))

# Plot data
################################################################################

# Plot data
g <- ggplot(shellfish, aes(x=sample_date, y=location, color=species, size=da_ppm)) +
  geom_point() +
  # Label
  labs(x="Sample date", y="", title="ODA 2000-2015 shellfish biotoxin sampling") +
  scale_x_date(breaks=seq(ymd("2000-01-01"), ymd("2021-01-01"), by="1 year"),
               labels=2000:2021) +
  scale_color_discrete(name="Species") +
  scale_size_continuous(name="Domoic acid\ncontamination (ppm)", range = c(0.3,3)) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=6),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
        plot.title=element_text(size=9),
        # Gridlines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "right")
g

ggsave(g, filename=file.path(plotdir, "OR_2010_2015_shellfish_da_samples.png"),
       width=6.5, height=4.5, units="in", dpi=600)

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(toxin=analyte_name, product_orig=product,
         quantity_operator=quantitative_operator, quantity_units=quantitative_unit,
         quantity_comments=z_analyte_comment_ct,
         sample_date=date, sample_time=x4) %>%
  # Format date
  mutate(sample_date=as.character(sample_date),
         sample_date=recode(sample_date, "2027-02-23"="2017-02-23", "2000-07-06"="2011-07-06"),
         sample_date=lubridate::ymd(sample_date)) %>%
  # Format product
  # mutate(product=tolower(product),
  #        product=recode(product,
  #                      "\"mini\" razors"="Mini razors",
  #                      "(ca) mussels"="California mussels",
  #                      "asian clams"="Asian clams",
  #                      "asian varnish"="Asian varnish clams",
  #                      "asian varnish clams"="Asian varnish clams",
  #                      "bay clams"="Bay clams",
  #                      "bay crab viscera - coos bay"="Bay crab - viscera - Coos Bay",
  #                      "bay mussels"="Bay mussels",
  #                      "butter clams"="Butter mussels",
  #                      "ca mussel"="California mussels",
  #                      "ca mussels"="California mussels",
  #                      "ca. mussels"="California mussels",
  #                      "caifornia mussels"="California mussels",
  #                      "california blue mussels"="California blue mussels",
  #                      "california mussels"="California mussels",
  #                      "california mussles"="California mussels",
  #                      "clams"="Clams",
  #                      "clams, butter"="Butter mussels",
  #                      "clams, cockle"="Cockle clams",
  #                      "clams, gaper"="Gaper clams",
  #                      "clams, purple varnish"="Purple varnish clams",
  #                      "clams, razor"="Razor clams",
  #                      "clams, softshell"="Softshell clams",
  #                      "clams,razor"="Razor clams",
  #                      "clams. razor"="Razor clams",
  #                      "cockle clams"="Cockle clams",
  #                      "cockles"="Cockles",
  #                      "cooked crab viscera - brookings"="Crab - viscera (cooked) - Brookings",
  #                      "cooked crab viscera - coos bay"="Crab - viscera (cooked) - Coos Bay",
  #                      "cooked crab viscera - garibaldi"="Crab - viscera (cooked) - Garibaldi",
  #                      "cooked crab viscera - newport"="Crab - viscera (cooked) - Newport",
  #                      "cooked crab viscera - port orford"="Crab - viscera (cooked) - Port Orford",
  #                      "cooked crab viscera-off depoe bay"="Crab - viscera (cooked) - Off Depoe Bay",
  #                      "cooked crab viscera-tillamook"="Crab - viscera (cooked) - Tillamook",
  #                      "cooked dungeness viscera - astoria"="Dungeness crab - viscera (cooked) - Astoria",
  #                      "crab leg meat"="Crab - legs",
  #                      "crab legs"="Crab - legs",
  #                      "crab viscera"="Crab - viscera",
  #                      "crab viscera - astoria"="Crab - viscera - Astoria",
  #                      "crab viscera - brookings"="Crab - viscera - Brookings",
  #                      "crab viscera - coos bay"="Crab - viscera - Coos Bay",
  #                      "crab viscera - flores creek"="Crab - viscera - Flores Creek",
  #                      "crab viscera - garibaldi"="Crab - viscera - Garibaldi",
  #                      "crab viscera - newport"="Crab - viscera - Newport",
  #                      "crab viscera - port orford"="Crab - viscera - Port Orford",
  #                      "crab viscera/crab legs"="Crab - viscera/legs",
  #                      "crab viscera/legs"="Crab - viscera/legs",
  #                      "crab visera"="Crab - viscera",
  #                      "crab, dugeness"="Dungeness crab",
  #                      "crab, dungeness"="Dungeness crab",
  #                      "crab, dungeness - leg meat"="Dungeness crab - leg meat",
  #                      "crab, dungeness - viscera"="Dungeness crab - viscera",
  #                      "crab, dungeness - viscers"="Dungeness crab - viscera",
  #                      "crab, dungeness - visera"="Dungeness crab - viscera",
  #                      "crab, dungeness -viscera"="Dungeness crab - viscera",
  #                      "crab, dungeness viscera"="Dungeness crab - viscera",
  #                      "crab, dungeness-viscera"="Dungeness crab - viscera",
  #                      "crab, dungneness - viscera"="Dungeness crab - viscera",
  #                      "crabs, dungeness"="Dungeness crab",
  #                      "d. crab viscera"="Dungeness crab - viscera",
  #                      "dungeness crab viscera"="Dungeness crab - viscera",
  #                      "dungeness crab viscera (cooked)"="Dungeness crab - viscera (cooked)",
  #                      "e thin clams"="Eastern thinshell clams",
  #                      "e thinshell clams"="Eastern thinshell clams",
  #                      "e. thinshell clams"="Eastern thinshell clams",
  #                      "ea thinshell clams"="Eastern thinshell clams",
  #                      "eastern softshell clam"="Eastern softshell clams",
  #                      "eastern thinshell clam"="Eastern thinshell clams",
  #                      "eastern thinshell clams"="Eastern thinshell clams",
  #                      "eastern thinshell mussels"="Eastern thinshell mussels",
  #                      "gaper clams"="Gaper clams",
  #                      "littleneck clams"="Littleneck clams",
  #                      "musels"="Mussels",
  #                      "mussel"="Mussels",
  #                      "mussels"="Mussels",
  #                      "mussels in shell"="Mussels - in shell",
  #                      "mussles"="Mussels",
  #                      "ocean crab viscera - coos bay"="Ocean crab - viscera - Coos Bay",
  #                      "oregon gaper clams"="Oregon gaper clams",
  #                      "oysters"="Oysters",
  #                      "purple vanish clams"="Purple varnish clams",
  #                      "purple varnish clams"="Purple varnish clams",
  #                      "razor clam"="Razor clams",
  #                      "razor clams"="Razor clams",
  #                      "razor clams yearlings"="Razor clams - yearlings",
  #                      "razors"="Razor clams",
  #                      "rzor clams"="Razor clams",
  #                      "thinshell clams"="Thinshell clams",
  #                      "tsunami debris mussels"="Tsunami debris mussels",
  #                      "unknown product description"="Unknown",
  #                      "varnish clams"="Varnish clams")) %>%
  # Format quantity
  mutate(quantity=as.numeric(quantity)) %>%
  # mutate(
  #        # Fix missing values for <5.5
  #        # quantity_operator=ifelse(grepl("<5.5|< 5.5", quantity_comments), "<", quantity_operator),
  #        # quantity=ifelse(grepl("<5.5|< 5.5", quantity_comments), 5.5, quantity),
  #        # # Fix missing values for <5.5
  #        # quantity_operator=ifelse(grepl("<1.0|< 1.0", quantity_comments), "<", quantity_operator),
  #        # quantity=ifelse(grepl("<1.0|< 1.0", quantity_comments), 1, quantity),
  #        # # Fix missing values for <2.0
  #        # quantity_operator=ifelse(grepl("<2.0", quantity_comments), "<", quantity_operator),
  #        # quantity=ifelse(grepl("<2.0", quantity_comments), 2, quantity),
  #        # Convert to numeric
  #        quantity=as.numeric(quantity)) %>%
  # Add product info
  # left_join(product_key, by="product") %>%
  # Format location
  # mutate(location=ifelse(location=="Crab Viscera- General History" & !is.na(location2), location2, location)) %>%
  # select(-location2) %>%
  # Add location info
  # rename(location_orig=location) %>%
  # left_join(site_key %>% select(location_orig, location, lat_dd, long_dd), by="location_orig") %>%
  # Arrange
  select(product_orig, #comm_name_orig, comm_name, sci_name, type,
         sample_date, sample_time,
         #location_orig, location, lat_dd, long_dd,
         toxin,
         quantity_operator, quantity, quantity_units, quantity_comments, everything()) %>%
  arrange(product_orig, sample_date, sample_time, location, toxin)

# Inspect data
str(data)
freeR::complete(data) # NAs in the following are okay: sci name, sample time, operator, quantity (maybe), lat/long (1 only)

# Inspect values
table(data$location)
range(data$sample_date)
table(data$toxin)
table(data$quantity_operator)
table(data$quantity_units)

# Inspect missing quantities
comments <- data %>%
  filter(is.na(quantity)) %>%
  select(quantity_comments) %>%
  unique() %>%
  mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
  unique() %>%
  mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
  unique() %>%
  mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
  unique() %>%
  mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
  mutate(quantity_comments=stringr::str_trim(quantity_comments)) %>%
  unique() %>%
  arrange(quantity_comments)

# Common name harmonization
species_key <- data %>%
  group_by(comm_name, sci_name) %>%
  summarize(comm_name_orig=paste(sort(unique(comm_name_orig)), collapse=", ")) %>%
  ungroup()

# Export
write.csv(species_key, file=file.path(outdir, "species_key.csv"), row.names=F)
write.csv(comments, file=file.path(outdir, "comments_for_missing_quantities.csv"), row.names=F)


# Build site key
################################################################################

# Build site key
site_key_messy <- data %>%
  group_by(location_orig) %>%
  summarize(species=paste(sort(unique(product)), collapse=", "))

# Export site key
write.csv(site_key_messy, file=file.path(outdir, "OR_da_sampling_site_key_incomplete.csv"), row.names=F)



# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "ODA_2010_2020_da_sampling_data.Rds"))



# Plot data
################################################################################

# Read data
zones <- readxl::read_excel(file.path(outdir, "dcrab_da_mgmt_zones.xlsx"))
zone_lats <- c(zones$lat_dd_north[1], zones$lat_dd_south)

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.title=element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    axis.text.y = element_text(angle = 90, hjust = 0.5))


# Plot data coverage
g <- ggplot(data, mapping=aes(x=sample_date, y=lat_dd)) +
  facet_wrap(~comm_name, ncol=2) +
  geom_point() +
  labs(x="Sample data", y="Latitude (°N)") +
  theme_bw() + base_theme
g


# Dungeness crab data
################################################################################

# Oregon Dungeness crab season
# December 1st to August 14
openers <- paste0(2010:2020, "-12-01") %>% ymd()
closers <- paste0(2011:2021, "-08-14") %>% ymd()
season_key <- tibble(year=paste(2010:2020, 2011:2021, sep="-"),
                     open=openers,
                     close=closers)

# Dungeness crab
dcrab <- data %>%
  filter(comm_name=="Dungeness crab")
dcrab_na <- dcrab %>%
  filter(is.na(quantity))

# Plot data coverage
g1 <- ggplot(dcrab, mapping=aes(x=sample_date, y=lat_dd, color=type, size=quantity)) +
  # Seasons
  geom_rect(data=season_key, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=47, fill="grey90") +
  # Zones
  geom_hline(yintercept=zone_lats, linetype="dotted") +
  # geom_rect(data=dcrab_season, inherit.aes=F,
  #         mapping=aes(xmin=open, xmax=close), ymin=0, ymax=1, fill="grey90") +
  # Sampling events
  geom_point() +
  geom_point(data=dcrab_na, mapping=aes(x=sample_date, y=lat_dd), shape="x", inherit.aes=F) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Oregon Dungeness crab biotoxin sampling") +
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-12-31"), by="1 year"), labels=2010:2021) +
  # Legends
  scale_color_discrete(name="Sample type") +
  scale_size_continuous(name="Domoic acid\ncontamintion (ppm)") +
  # Theme
  theme_bw() + base_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "OR_dcrab_da_samples.png"),
       width=6.5, height=3.5, units="in", dpi=600)






