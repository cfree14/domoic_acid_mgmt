

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(raster)
library(lubridate)
library(tidyverse)

# Directories
datadir1 <- "data/california/pier_sampling/data"
datadir2 <- "data/merged/processed"
plotdir <- "figures"

# Read pier sampling data
pn_orig <- readRDS(file.path(datadir1, "2014_2021_pn_density_by_pier_for_plotting.Rds"))
pda_orig <- readRDS(file.path(datadir1, "2014_2021_pda_density_by_pier_for_plotting.Rds"))

# Read pier sampling sites
sites <- read.csv(file=file.path(datadir2, "WC_pn_pda_beach_pier_sampling_sites.csv"), as.is=T)

# Read C-HARM data
charmdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid/data/charm/processed/"
charm_hov_orig <- readRDS(file.path(charmdir, "CHARM_20140305_to_present_imputed_hovmoller_data_for_plotting.Rds"))

# Read C-HARM time series
charm_pda <- raster::brick(file.path(charmdir, "CHARM_DAP_20140305_to_present_imputed.grd"))

# Format observation data
################################################################################

# Format PN data
pn <- pn_orig %>%
  filter(location=="Monterey Wharf") %>%
  mutate(pn_group=factor(pn_group, levels=c("Seriata", "Delicatissima")))

# Format PN data
pda <- pda_orig %>%
  filter(location %in% c("Monterey Wharf"))

# Central Region season key
season_key_c <- tibble(region="Central",
                       year1=2013:2020,
                     year2=2014:2021) %>%
  # Add dates
  mutate(season=paste(year1, year2-2000, sep="-"),
         open_date=paste0(year1, "-11-15"),
         close_date=paste0(year2, "-06-30")) %>%
  # Recode 2014 season date
  mutate(open_date=ifelse(year1==2013, "2014-01-01", open_date)) %>%
  # Convert to date
  mutate(open_date=ymd(open_date),
         close_date=ymd(close_date))

# Northern Region season key
season_key_n <- tibble(region="Northern",
                       year1=2013:2020,
                       year2=2014:2021) %>%
  # Add dates
  mutate(season=paste(year1, year2-2000, sep="-"),
         open_date=paste0(year1, "-12-01"),
         close_date=paste0(year2, "-07-15")) %>%
  # Recode 2014 season date
  mutate(open_date=ifelse(year1==2013, "2014-01-01", open_date)) %>%
  # Convert to date
  mutate(open_date=ymd(open_date),
         close_date=ymd(close_date))

# Merge season keys
season_key <- bind_rows(season_key_c, season_key_n)


# Format prediction data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Northern dates
dates_n <- purrr::map(1:nrow(season_key_n), function(x){
  open <- season_key_n$open_date[x]
  close <- season_key_n$close_date[x]
  dates <- seq(open, close, by="1 day") %>% as.character()
}) %>% unlist() %>% ymd()

# Central dates
dates_c <- purrr::map(1:nrow(season_key_c), function(x){
  open <- season_key_c$open_date[x]
  close <- season_key_c$close_date[x]
  dates <- seq(open, close, by="1 day") %>% as.character()
}) %>% unlist() %>% ymd()

# Format C-HARM Hovmoller data
charm_hov <- charm_hov_orig %>%
  # Days of interest
  filter(date>="2014-01-01", date <="2021-07-15") %>%
  # North of Point Conception
  filter(lat_dd >= 34) %>%
  # Label weather or not in season
  mutate(season="out-of-season",
         season=ifelse(lat_dd>=son_mend_county & date %in% dates_n, "in season", season),
         season=ifelse(lat_dd<son_mend_county & date %in% dates_c, "in season", season),
         season=factor(season, c("in season", "out-of-season"))) # %>%
  # Sample
  # sample_frac(0.)

# Format C-HARM raster for plotting
data_do <- "2018-04-01"
data_do_label <- data_do %>% gsub("-", ".", .) %>% paste0("X", .)
charm_df <- charm_pda[[data_do_label]] %>%
  raster::as.data.frame(., xy=T) %>%
  setNames(c("long_dd", "lat_dd", "pda"))


# Plot data
################################################################################

# Turn off scientific notation for plotting
options(scipen=999)

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   plot.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")

# Plot pier sampling sites
g1 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot sampling sites
  geom_point(data=sites, mapping=aes(x=long_dd, lat_dd), size=1) +
  geom_text(data=sites %>% filter(site=="Monterey Wharf"), mapping=aes(x=long_dd, lat_dd, label=site), size=2, hjust=-0.1) +
  # Labels
  labs(x="", y="", title="Beach and pier monitoring", tag="A") +
  scale_y_continuous(breaks=seq(32,48,2)) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(32, 48)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank())
g1

# Plot C-HARM forecasts
g2 <- ggplot() +
  # Plot raster
  geom_raster(data=charm_df, mapping=aes(x=long_dd, y=lat_dd, fill=pda)) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(x="", y="", title="HAB nowcasts and forecasts", tag="D") +
  scale_y_continuous(breaks=seq(32,48,2)) +
  # Legend
  scale_fill_gradientn(name="pDA risk", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-127, -116.6), ylim = c(32, 48)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        legend.position = c(0.16, 0.82),
        legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill=alpha('blue', 0)),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# Plot PN data
# Clarissa's bloom threshold is 10^4 cells / L
ymax <- max(pn$cells_l_max, na.rm=T) + 1
g3 <- ggplot() +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date), ymin=-10, ymax=ymax, fill="grey90") +
  # Lines
  geom_line(data=pn, mapping=aes(x=date_std, y=cells_l_max+1, color=pn_group), lwd=0.4) +
  # Bloom threshhold reference line
  geom_hline(yintercept=c(10^4), linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Cell\ndensity (cells/L)", tag="B",
       title=expression("Historical observations of "*italic("Pseudo-nitzschia")*" at Monterey Wharf")) +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10",
                     breaks=10^c(0:5),
                     labels=parse(text=paste0("10^", 0:5))) +
  # Legend
  scale_color_discrete(name="Size group") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.897, 0.2),
        legend.key.size = unit(0.3, "cm"),
        legend.key = element_rect(fill=alpha('blue', 0)),
        legend.background = element_rect(fill=alpha('blue', 0)))
g3

# Plot pDA data
# Clarissa's threshold of evevated pDA is 0.5 ug/L = 0.5 ng/ml
# 0.5 ug/L * (1 L / 1000 mL) * (1 g / 10^6 ug) * (10^9 ng / 1 g)
0.5  / 1000 / 10^6 * 10^9
ymax <- max(pda$pda_ng_ml_max, na.rm=T) + 0.00001
g4 <- ggplot() +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date), ymin=-10, ymax=ymax, fill="grey90") +
  # Lines
  geom_line(data=pda, mapping=aes(x=date_std, y=pda_ng_ml_max+0.00001), lwd=0.4) +
  # Elevated pDA reference line
  geom_hline(yintercept=0.5, linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Particulate\ndomoic acid (ng/ml)", tag="C",
       title="Historical observations of particulate domoic acid at Monterey Wharf") +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10",
                     breaks=10^c(1:-5),
                     # labels=paste0("10^", 1:-5),
                     labels=parse(text=paste0("10^", 1:-5))) +
  # Theme
  theme_bw() + my_theme
g4

# Plot C-HARM pDA
g5 <- ggplot(charm_hov %>% filter(variable=="Particulate domoic acid (pDA)"),
            aes(x=date, y=lat_dd, fill=risk_avg, alpha=season)) +
  geom_raster() +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(breaks=seq(34,42,2)) +
  # Labels
  labs(x="", y=" \nLatitude (°N)", tag="E",
       title="Historical predictions of particulate domoic acid risk from C-HARM") +
  # Legend
  scale_fill_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  scale_alpha_manual(name="", values=c(1, 0.3)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g5

# Plot C-HARM pDA
g6 <- ggplot(charm_hov %>% filter(variable=="Cellular domoic acid (cDA)"),
             aes(x=date, y=lat_dd, fill=risk_avg, alpha=season)) +
  geom_raster() +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(breaks=seq(34,42,2)) +
  # Labels
  labs(x="", y=" \nLatitude (°N)", tag="F",
       title="Historical predictions of cellular domoic acid risk from C-HARM") +
  # Legend
  scale_fill_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  scale_alpha_manual(name="", values=c(1, 0.3)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g6

# Merge plots
layout_matrix <- matrix(data=c(1, 3,
                               1, 4,
                               2, 5,
                               2, 6), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6,
                             layout_matrix=layout_matrix, widths=c(0.3, 0.7))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_midseason_risk_evidence.png"),
       width=6.5, height=7, units="in", dpi=600)

