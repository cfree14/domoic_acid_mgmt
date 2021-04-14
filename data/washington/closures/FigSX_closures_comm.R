

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plotly)
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/washington/raw_data/Files for Chris Free"
outdir <- "data/washington/closures/data"
plotdir <- "data/washington/closures/figures/temp"
gisdir <- "data/washington/gis_data/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "WA_DOH_2014_2020_biotoxin_closures.Rds"))

# Read zone key
zone_key <- readxl::read_excel(file.path(gisdir, "WDFW_closure_zone_key.xlsx")) %>%
  mutate(zone_id=as.character(zone_id),
         zone_id_chr=ifelse(nchar(zone_id) < 5, stringr::str_pad(zone_id, width=5, side="left"), zone_id),
         zone_label=paste(zone, zone_id_chr, sep=" - ")) %>%
  arrange(basin, zone_label)


# Read CGA key
cga_key <- read.csv(file.path(gisdir, "WDFW_commercial_growing_areas_key.csv"), as.is=T)


# Plot closure zones
################################################################################

# Add zone label to data
data <- data_orig %>%
  left_join(zone_key %>% select(zone_id, zone_label)) %>%
  mutate(zone_label=factor(zone_label, levels=zone_key$zone_label))
n_distinct(data$zone_label)

# Species
species <- sort(unique(data$species))

# Loop through species
i <- 1
for(i in 1:length(species)){

  # Species
  spp_do <- species[i]
  print(paste(i, spp_do))

  # Data lines
  data_lines <- data %>%
    # Reduce
    filter(species==spp_do & zone_type=="Closure zone") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, zone_id, zone_label, date1, date2) %>%
    # Reduce to complete lines
    filter(!is.na(date1) & !is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 10:11) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open"))

  # Data points
  data_points <- data %>%
    # Reduce
    filter(species==spp_do & zone_type=="Closure zone") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, zone_id, zone_label, date1, date2) %>%
    # Reduce to complete lines
    filter(is.na(date1) | is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 10:11) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open")) %>%
    # Remove missing
    filter(!is.na(date))

  # Plot simple data
  g <- ggplot(data_lines, aes(x=date, y=zone_label, color=fishery, group=event_id)) +
    # facet_grid(basin~., scales="free", space="free") +
    ggforce::facet_col(vars(basin), scales = "free_y", space = "free", drop=F) +
    geom_line() +
    # Start/end points
    geom_point(data=data_points, aes(x=date, y=zone_label, color=fishery, shape=date_type), inherit.aes = F) +
    # Labs
    labs(x="", y="", title=paste(spp_do, "closures")) +
    scale_color_discrete(name="Fishery") +
    scale_shape_manual(name="Decision", values=c(16, 21)) +
    scale_x_date(breaks=seq(ymd("2014-01-01"), ymd("2021-01-01"), by="1 year"), labels=2014:2021) +
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
  outfile <- paste0("FigSX_closures_rec_", tolower(spp_do) %>% gsub(" ", "_", .), ".pdf")
  ggsave(g, filename=file.path(plotdir, outfile),
         width=6.5, height=13, units="in", dpi=300)

}


# Plot commercial growing areas
################################################################################

# Add zone label to data
data <- data_orig %>%
  left_join(zone_key %>% select(zone_id, zone_label)) %>%
  mutate(zone_label=factor(zone_label, levels=zone_key$zone_label))
n_distinct(data$zone_label)

# Species
species <- sort(unique(data$species[data$zone_type=="Commercial growing area"]))

# Loop through species
i <- 1
for(i in 1:length(species)){

  # Species
  spp_do <- species[i]
  print(paste(i, spp_do))

  # Data lines
  data_lines <- data %>%
    # Reduce
    filter(species==spp_do & zone_type=="Commercial growing area") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, zone_id, zone_label, date1, date2) %>%
    # Reduce to complete lines
    filter(!is.na(date1) & !is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 10:11) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open"))

  # Data points
  data_points <- data %>%
    # Reduce
    filter(species==spp_do & zone_type=="Commercial growing area") %>%
    # Simplify
    select(event_id, species, fishery, status, basin, zone_type, zone, zone_id, zone_label, date1, date2) %>%
    # Reduce to complete lines
    filter(is.na(date1) | is.na(date2)) %>%
    # Gather
    gather(key="date_type", value="date", 10:11) %>%
    mutate(date_type=recode(date_type,
                            "date1"="Close", "date2"="Open")) %>%
    # Remove missing
    filter(!is.na(date))

  # Plot simple data
  g <- ggplot(data_lines, aes(x=date, y=zone, color=fishery, group=event_id)) +
    # facet_grid(basin~., scales="free", space="free") +
    ggforce::facet_col(vars(basin), scales = "free_y", space = "free", drop=F) +
    geom_line() +
    # Start/end points
    geom_point(data=data_points, aes(x=date, y=zone_label, color=fishery, shape=date_type), inherit.aes = F) +
    # Labs
    labs(x="", y="", title=paste(spp_do, "closures")) +
    scale_color_discrete(name="Fishery") +
    scale_shape_manual(name="Decision", values=c(16, 21)) +
    scale_x_date(breaks=seq(ymd("2014-01-01"), ymd("2021-01-01"), by="1 year"), labels=2014:2021) +
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
  outfile <- paste0("FigSX_closures_comm_", tolower(spp_do) %>% gsub(" ", "_", .), ".pdf")
  ggsave(g, filename=file.path(plotdir, outfile),
         width=6.5, height=6, units="in", dpi=300)

}

