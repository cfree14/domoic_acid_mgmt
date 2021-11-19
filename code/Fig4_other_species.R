
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
ordir <- "data/oregon/processed"
outdir <- "data/merged/processed"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))

# Species groups
spp_group_key <- readxl::read_excel(file.path(tabledir, "TableS3_species_sample_sizes.xlsx")) %>%
  setNames(c("full_name", "states", "nsamples", "species_group")) %>%
  # Extract common name
  mutate(comm_name=sub(" \\(.*", "", full_name)) %>%
  # Fix a few
  mutate(comm_name=recode(comm_name,
                          "Mediterranean"="Mediterranean (bay) mussel",
                          "California"="California mussel",
                          "California spiny lobster"="Spiny lobster",
                          "Gaper"="Gaper (horse) clam",
                          "Sheep"="Sheep (spider) crab"))


# Season info
################################################################################

# Seasons
seasons_do <- 2014:2020

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Washington season
# December 1st to September 15th
openers_wa <- paste0(seasons_do, "-12-01") %>% ymd()
closers_wa <- paste0(seasons_do+1, "-09-15") %>% ymd()
seasons_wa <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_wa,
                     close=closers_wa)

# Oregon season
# December 1st to August 14th
openers_or <- paste0(seasons_do, "-12-01") %>% ymd()
closers_or <- paste0(seasons_do+1, "-08-14") %>% ymd()
seasons_or <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_or,
                     close=closers_or)

# California-North season
# December 1st to July 15th
openers_ca_n <- paste0(seasons_do, "-12-01") %>% ymd()
closers_ca_n <- paste0(seasons_do+1, "-07-15") %>% ymd()
seasons_ca_n <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_n,
                       close=closers_ca_n)

# California-Central season
# November 15th to June 30th
openers_ca_c <- paste0(seasons_do, "-11-15") %>% ymd()
closers_ca_c <- paste0(seasons_do+1, "-06-30") %>% ymd()
seasons_ca_c <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_c,
                       close=closers_ca_c)


# Build data
################################################################################

# DA cap
da_cap <- 500

# Data
data <- data_orig %>%
  # Species of interest
  filter(comm_name!="Dungeness crab" & date >= ymd("2014-01-01") & !is.na(date) & !is.na(lat_dd) & !is.na(da_ppm)) %>%
  # Set 0 to 1
  mutate(da_ppm=ifelse(da_ppm==0, 1, da_ppm)) %>%
  # Recode species
  mutate(comm_name=recode(comm_name,
                          "California spiny lobster"="Spiny lobster",
                          "California (sea) mussel"="California mussel")) %>%
  # Cap ppm
  mutate(da_ppm_cap=pmin(da_ppm, da_cap))

# Max contamination
max(data$da_ppm)

# Other species
data_other <- data %>%
  # Reduce to other species
  filter(!comm_name %in% c("Razor clam", "California mussel", "Dungeness crab")) %>%
  # Add species group
  left_join(spp_group_key %>% select(comm_name, species_group)) %>%
  # Order species groups
  mutate(species_group=factor(species_group,
                              levels=c("Rock crab", "Bay mussel", "Pacific oyster",
                                       "Spiny lobster", "Sardine/anchovy",
                                       "Other fish", "Other crab", "Other mollusc",
                                       "Other/unknown")))

freeR::complete(data_other)

# Other labels
other_labels <- data_other %>%
  # Calculate median dates/lats
  group_by(species_group) %>%
  summarize(n=n(),
            lat_dd=median(lat_dd),
            date=median(date)) %>%
  ungroup() %>%
  # Recode lats
  mutate(lat_dd=case_when(species_group=="Rock crab" ~ 37.3,
                          species_group=="Pacific oyster" ~ 36,
                          species_group=="Sardine/anchovy" ~ 36,
                          species_group=="Other mollusc" ~ 42.8,
                          TRUE ~ lat_dd)) %>%
  # Recode dates
  # Recode lats
  mutate(date=case_when(species_group=="Rock crab" ~ "2017-02-20" %>% ymd(),
                        species_group=="Pacific oyster" ~ "2019-01-01" %>% ymd(),
                        species_group=="Sardine/anchovy" ~ "2014-06-01" %>% ymd(),
                        species_group=="Other mollusc" ~ "2014-06-01" %>% ymd(),
                        TRUE ~ date)) %>%
  # Remove uninteresting ones
  # filter(n>100)
  mutate(lat_dd=ifelse(n<100, NA, lat_dd))

# Build stars
stars <- matrix(data=c("Razor clam", "2019-03-15", 39.85,
                       "California mussel", "2017-04-25", 36.0,
                       "Other species", "2015-03-10", 37.0,
                       "Other species", "2016-03-01", 37.6),
                ncol=3, byrow=T, dimnames = list(NULL, c("comm_name", "date", "lat_dd"))) %>%
  as.data.frame() %>%
  mutate(date=ymd(date),
         lat_dd=as.numeric(lat_dd))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x = element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   plot.tag=element_text(size=8),
                   plot.tag.position = c(0.007, 0.98),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

# Plot data
g1 <- ggplot(data %>% filter(comm_name=="Razor clam"),
            aes(x=date, y=lat_dd, size=da_ppm, fill=da_ppm_cap)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8, pch=21, color="black", stroke=0.1) +
  # Management zone lines
  # geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
  #              inherit.aes = F, color="grey40", size=0.35) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot stars
  ggstar::geom_star(data=stars %>% filter(comm_name=="Razor clam"), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=3, fill="black", color=NA) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="Razor clam", tag="A") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)",
                        range = c(0.5, 5), lim=c(1, da_cap), breaks=c(1, 20, 50, 100, 200, 500)) +
  scale_fill_gradientn(name="Domoic acid (ppm)",
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                        lim=c(1, da_cap), guide="none",
                        breaks=c(1, 5, 10, 20, 50, 100, 200, 500), trans="log2") +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
g2 <- ggplot(data %>% filter(comm_name=="California mussel"),
             aes(x=date, y=lat_dd, size=da_ppm, fill=da_ppm_cap)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8, pch=21, color="black", stroke=0.1) +
  # Management zone lines
  # geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
  #              inherit.aes = F, color="grey40", size=0.35) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot stars
  ggstar::geom_star(data=stars %>% filter(comm_name=="California mussel"), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=3, fill="black", color=NA) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="California mussel", tag="B") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)", range = c(0.5, 5), lim=c(1, da_cap), guide="none", breaks=c(1, 20, 50, 100, 200, 500)) +
  scale_fill_gradientn(name="Domoic acid (ppm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(1, da_cap),
                       breaks=c(1, 5, 10, 20, 50, 100, 200, 500), trans="log2") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g2

# Plot data
g3 <- ggplot(data_other,
             aes(x=date, y=lat_dd, size=da_ppm_cap, fill=species_group)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Sampling points
  geom_point(alpha=0.8, pch=21, color="black", stroke=0.1) +
  # Label species groups
  geom_text(data=other_labels, mapping=aes(x=date, y=lat_dd,
                                           label=species_group, color=species_group),
            inherit.aes = F, size=2.1, hjust=0, show.legend = F) +
  # Management zone lines
  # geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
  #              inherit.aes = F, color="grey40", size=0.35) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot stars
  ggstar::geom_star(data=stars %>% filter(comm_name=="Other species"), mapping=aes(x=date, y=lat_dd), inherit.aes = F, size=3, fill="black", color=NA) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)", title="All other species", tag="C") +
  # Legends
  scale_size_continuous(name="Domoic acid (ppm)", range = c(0.5, 5), lim=c(1, da_cap), guide="none",
                        breaks=c(1, 20, 50, 100, 200, 500)) +
  scale_fill_discrete(name="Species group") +
  # Theme
  theme_bw() + my_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1)

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig4_other_species.png"),
       width=6.5, height=6.5, units="in", dpi=600)

