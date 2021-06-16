
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
closuredir <- "data/closures/processed"
plotdir <- "data/closures/figures"

# Read data
data_orig <- readRDS(file.path(closuredir, "CDFW_2015_2021_comm_dcrab_closures.Rds"))


# Format data
################################################################################

# Fix data
data <- data_orig %>%
  # Recode status
  mutate(status=as.character(status),
         status=ifelse(status=="Out-of-season", NA, status),
         status=recode(status, "Whale entanglement closure"="Marine life entanglement closure"),
         status=factor(status, levels=c("Season open", "Body condition delay",
                                        "Body condition/domoic acid delay", "Domoic acid delay",
                                        "Evisceration order", "Marine life entanglement closure", "Oil spill")))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Limits
  # scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # State/region lines
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Labels
  labs(x="Date", y="Latitude (°N)", title="CDFW Commercial Dungeness crab closures") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy", "blue"), na.translate = F, drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_cdfw_comm_dcrab_closures.png"),
       width=6.5, height=4, units="in", dpi=600)


