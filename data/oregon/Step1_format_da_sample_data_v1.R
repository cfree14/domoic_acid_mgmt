

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
workdir <- "data/oregon/intermediate"
plotdir <- "data/oregon/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "OR.DA.xlsx"))
clams_orig <- readxl::read_excel(file.path(indir, "clams.xlsx"))
mussels_orig <- readxl::read_excel(file.path(indir, "mussels.xlsx"))

# Read product key
product_key <- readxl::read_excel(file.path(workdir, "product_key.xlsx")) %>%
  rename(location2=location)



# 1) Format clams/mussels data
################################################################################

# Format data
clams <- clams_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(date=sampled_date, location=collection_site,
         da_oper=vari_for_da, da_ppm=domoic_acid, psp_oper=vari_for_psp, psp_ppm=psp_toxins) %>%
  mutate(comm_name="Razor clam",
         species="Siliqua patula")

# Format data
mussels <- mussels_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(date=sampled_date, location=collection_site,
         da_oper=vari_for_da, da_ppm=domoic_acid, psp_oper=vari_for_psp, psp_ppm=psp_toxins) %>%
  mutate(comm_name="California mussel",
         species="Mytilus californianus")

# Merge shellfish
shellfish <- bind_rows(clams, mussels) %>%
  # Format date
  mutate(date=ymd(date)) %>%
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
  select(-location_orig) %>%
  select(comm_name, species, date, location, everything()) %>%
  arrange(comm_name, species, date, location)

# Inspect data
str(shellfish)
freeR::complete(shellfish)

# Inspect more
range(shellfish$date)
table(shellfish$da_oper)
table(shellfish$psp_oper)
table(shellfish$location)

# Plot data
g <- ggplot(shellfish, aes(x=date, y=location, color=comm_name, size=da_ppm)) +
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

# Export plot
ggsave(g, filename=file.path(plotdir, "OR_2010_2015_shellfish_da_samples.png"),
       width=6.5, height=4.5, units="in", dpi=600)

# Export
saveRDS(shellfish, file=file.path(workdir, "ODA_1999_2015_da_sampling_data_clams_mussels.Rds"))


# 2) Format all species data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(sample_id=lab_id,
         date=sample_date, time=sample_time,
         toxin=analyte_name,
         quantity_operator=quantitative_operator, quantity_units=quantitative_unit,
         quantity_comments=z_analyte_comment_ct) %>%
  # Format date
  mutate(date=as.character(date),
         date=recode(date, "2027-02-23"="2017-02-23", "2000-07-06"="2011-07-06"),
         date=lubridate::ymd(date),
         year=year(date),
         month=month(date)) %>%
  # Format product
  mutate(product=tolower(product),
         product=recode(product,
                       "\"mini\" razors"="Mini razors",
                       "(ca) mussels"="California mussels",
                       "asian clams"="Asian clams",
                       "asian varnish"="Asian varnish clams",
                       "asian varnish clams"="Asian varnish clams",
                       "bay clams"="Bay clams",
                       "bay crab viscera - coos bay"="Bay crab - viscera - Coos Bay",
                       "bay mussels"="Bay mussels",
                       "butter clams"="Butter mussels",
                       "ca mussel"="California mussels",
                       "ca mussels"="California mussels",
                       "ca. mussels"="California mussels",
                       "caifornia mussels"="California mussels",
                       "california blue mussels"="California blue mussels",
                       "california mussels"="California mussels",
                       "california mussles"="California mussels",
                       "clams"="Clams",
                       "clams, butter"="Butter mussels",
                       "clams, cockle"="Cockle clams",
                       "clams, gaper"="Gaper clams",
                       "clams, purple varnish"="Purple varnish clams",
                       "clams, razor"="Razor clams",
                       "clams, softshell"="Softshell clams",
                       "clams,razor"="Razor clams",
                       "clams. razor"="Razor clams",
                       "cockle clams"="Cockle clams",
                       "cockles"="Cockles",
                       "cooked crab viscera - brookings"="Crab - viscera (cooked) - Brookings",
                       "cooked crab viscera - coos bay"="Crab - viscera (cooked) - Coos Bay",
                       "cooked crab viscera - garibaldi"="Crab - viscera (cooked) - Garibaldi",
                       "cooked crab viscera - newport"="Crab - viscera (cooked) - Newport",
                       "cooked crab viscera - port orford"="Crab - viscera (cooked) - Port Orford",
                       "cooked crab viscera-off depoe bay"="Crab - viscera (cooked) - Off Depoe Bay",
                       "cooked crab viscera-tillamook"="Crab - viscera (cooked) - Tillamook",
                       "cooked dungeness viscera - astoria"="Dungeness crab - viscera (cooked) - Astoria",
                       "crab leg meat"="Crab - legs",
                       "crab legs"="Crab - legs",
                       "crab viscera"="Crab - viscera",
                       "crab viscera - astoria"="Crab - viscera - Astoria",
                       "crab viscera - brookings"="Crab - viscera - Brookings",
                       "crab viscera - coos bay"="Crab - viscera - Coos Bay",
                       "crab viscera - flores creek"="Crab - viscera - Flores Creek",
                       "crab viscera - garibaldi"="Crab - viscera - Garibaldi",
                       "crab viscera - newport"="Crab - viscera - Newport",
                       "crab viscera - port orford"="Crab - viscera - Port Orford",
                       "crab viscera/crab legs"="Crab - viscera/legs",
                       "crab viscera/legs"="Crab - viscera/legs",
                       "crab visera"="Crab - viscera",
                       "crab, dugeness"="Dungeness crab",
                       "crab, dungeness"="Dungeness crab",
                       "crab, dungeness - leg meat"="Dungeness crab - leg meat",
                       "crab, dungeness - viscera"="Dungeness crab - viscera",
                       "crab, dungeness - viscers"="Dungeness crab - viscera",
                       "crab, dungeness - visera"="Dungeness crab - viscera",
                       "crab, dungeness -viscera"="Dungeness crab - viscera",
                       "crab, dungeness viscera"="Dungeness crab - viscera",
                       "crab, dungeness-viscera"="Dungeness crab - viscera",
                       "crab, dungneness - viscera"="Dungeness crab - viscera",
                       "crabs, dungeness"="Dungeness crab",
                       "d. crab viscera"="Dungeness crab - viscera",
                       "dungeness crab viscera"="Dungeness crab - viscera",
                       "dungeness crab viscera (cooked)"="Dungeness crab - viscera (cooked)",
                       "e thin clams"="Eastern thinshell clams",
                       "e thinshell clams"="Eastern thinshell clams",
                       "e. thinshell clams"="Eastern thinshell clams",
                       "ea thinshell clams"="Eastern thinshell clams",
                       "eastern softshell clam"="Eastern softshell clams",
                       "eastern thinshell clam"="Eastern thinshell clams",
                       "eastern thinshell clams"="Eastern thinshell clams",
                       "eastern thinshell mussels"="Eastern thinshell mussels",
                       "gaper clams"="Gaper clams",
                       "littleneck clams"="Littleneck clams",
                       "musels"="Mussels",
                       "mussel"="Mussels",
                       "mussels"="Mussels",
                       "mussels in shell"="Mussels - in shell",
                       "mussles"="Mussels",
                       "ocean crab viscera - coos bay"="Ocean crab - viscera - Coos Bay",
                       "oregon gaper clams"="Oregon gaper clams",
                       "oysters"="Oysters",
                       "purple vanish clams"="Purple varnish clams",
                       "purple varnish clams"="Purple varnish clams",
                       "razor clam"="Razor clams",
                       "razor clams"="Razor clams",
                       "razor clams yearlings"="Razor clams - yearlings",
                       "razors"="Razor clams",
                       "rzor clams"="Razor clams",
                       "thinshell clams"="Thinshell clams",
                       "tsunami debris mussels"="Tsunami debris mussels",
                       "unknown product description"="Unknown",
                       "varnish clams"="Varnish clams")) %>%
  # Format quantity
  mutate(quantity1=as.numeric(quantity)) %>%
  # Extract quantity from quantity comments
  mutate(quantity2=gsub("PPM|/PPM|MRL = 1.0|PL=5.5|PL = 5.5|PL+5.5|P=5.5|MRL=5.5|NO TESTING PER J DOWELL|NO TESTING OER J DOWELL|MRL=2.0|MRL=1.0|<|. ",
                        "",
                        toupper(quantity_comments)),
         quantity2=stringr::str_trim(quantity2),
         quantity2=recode(quantity2, "PL+5. 5.5"="5.5"),
         quantity3=as.numeric(quantity2),
         quantity_diff=quantity3-quantity1,
         quantity=ifelse(!is.na(quantity1), quantity1, quantity3)) %>%
  select(-c(quantity1, quantity2, quantity3, quantity_diff)) %>%
  # Extract quantity operator from comments
  mutate(quantity_operator=ifelse(is.na(quantity_operator) & grepl("<", quantity_comments), "<", quantity_comments),
         quantity_operator=ifelse(is.na(quantity_operator), "", quantity_operator)) %>%
  # Add product info
  left_join(product_key, by="product") %>%
  # Format location
  mutate(location=ifelse(location=="Crab Viscera- General History" & !is.na(location2), location2, location)) %>%
  select(-location2) %>%
  # Arrange
  select(product, comm_name_orig, comm_name, sci_name, type,
         year, date, month, time,
         location,
         toxin, sample_id,
         quantity_operator, quantity, quantity_units, quantity_comments, everything()) %>%
  arrange(product, date, time, location, toxin, sample_id)

# Inspect data
str(data)
freeR::complete(data) # NAs in the following are okay: sci name, sample time, quantity (n=6), lat/long (n=1)

# Confirm no duplicated sample ids
anyDuplicated(data$sample_id)

# Inspect values
table(data$type)
table(data$location)
table(data$product)
sort(unique(data$product))
range(data$date)
table(data$toxin)
table(data$quantity_operator)
table(data$quantity_units)

# Common name harmonization
species_key <- data %>%
  group_by(comm_name, sci_name) %>%
  summarize(comm_name_orig=paste(sort(unique(comm_name_orig)), collapse=", "),
            n=n()) %>%
  ungroup() %>%
  arrange(desc(n))

# Export
write.csv(species_key, file=file.path(workdir, "species_key.csv"), row.names=F)

# # Inspect missing quantities
# comments <- data %>%
#   filter(is.na(quantity)) %>%
#   select(quantity_comments) %>%
#   unique() %>%
#   mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
#   unique() %>%
#   mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
#   unique() %>%
#   mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
#   unique() %>%
#   mutate(quantity_comments=gsub("  ", " ", quantity_comments)) %>%
#   mutate(quantity_comments=stringr::str_trim(quantity_comments)) %>%
#   unique() %>%
#   arrange(quantity_comments)
# write.csv(comments, file=file.path(workdir, "comments_for_missing_quantities.csv"), row.names=F)

# Plot data
g <- ggplot(data, aes(x=date, y=location, color=comm_name, size=quantity)) +
  # facet_grid(comm_name~., scales="free_y", space="free_y") +
  # facet_wrap(~comm_name, ncol=1, scales="free_y", strip.position="top", shrink=T) +
  ggforce::facet_col(vars(comm_name), scales = "free_y", space = "free") +
  geom_point() +
  # Label
  labs(x="Sample date", y="", title="ODA 2000-2015 biotoxin sampling") +
  scale_x_date(breaks=seq(ymd("2000-01-01"), ymd("2021-01-01"), by="1 year"),
               labels=2000:2021) +
  scale_color_discrete(name="Species", guide=F) +
  scale_size_continuous(name="Domoic acid\ncontamination (ppm)", range = c(0.3,3)) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=6),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
        plot.title=element_text(size=9),
        strip.text=element_text(size=6),
        # Gridlines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "right")
g

# Export plot
ggsave(g, filename=file.path(plotdir, "OR_2010_2015_shellfish_da_samples.png"),
       width=6.5, height=11, units="in", dpi=600)

# Export data
saveRDS(data, file=file.path(workdir, "ODA_2010_2020_da_sampling_data.Rds"))

