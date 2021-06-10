
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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_da_sampling_data.Rds"))


# Build data
################################################################################

# Data
data <- data_orig %>%
  filter(comm_name %in% c("Razor clam", "California mussel") & date >= ymd("2014-01-01") &
           !is.na(date) & !is.na(lat_dd) & !is.na(da_ppm)) %>%
  # Set 0 to 1
  mutate(da_ppm=ifelse(da_ppm==0, 1, da_ppm))

# Seasons
seasons_do <- 2014:2020


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=da_ppm)) +
  facet_wrap(~comm_name, nrow=2) +
  # Sampling points
  geom_point(pch=21, alpha=0.8, stroke=0.2) +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  # Label state lines
  annotate(geom="text", x=ymd("2014-01-01"), y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5, fontface=2) +
  annotate(geom="text", x=ymd("2014-01-01"), y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5, fontface=2) +
  annotate(geom="text", x=ymd("2014-01-01"), y=42, hjust=0, vjust=1.5, label="California", color="black", size=2.5, fontface=2) +
  # Limits
  scale_y_continuous(limits=c(32, 48.5), breaks=seq(32, 48, 2)) +
  scale_x_date(breaks=seq(ymd("2010-01-01"), ymd("2021-01-01"), by="1 year"), labels=2010:2021) +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Legends
  scale_fill_gradientn(name="Domoic acid (ppm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), trans="log10") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig8_bivalve_data_indiv.png"),
       width=6.5, height=5, units="in", dpi=600)


