
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
data_orig <- readRDS(file.path(closuredir, "ODFW_2011_2021_comm_dcrab_closures.Rds"))


# Format data
################################################################################

# Fix data
data <- data_orig %>%
  # Reduce to OR
  filter(lat_dd>42.00000 & lat_dd<=46.25000) %>%
  # Recode status
  mutate(status=as.character(status),
         status=recode(status,
                       "body condition"="Body condition delay",
                       "body condition/domoic acid"="Body condition/domoic acid delay",
                       "domoic acid"="Domoic acid delay",
                       "open"="Season open",
                       "out-of-season"="Out-of-season",
                       "evisceration order"="Evisceration order",
                       "evisceration order-OR vessels in WA waters"="Evisceration order")) %>%
  # Recode status
  mutate(status=recode(status, "Whale entanglement closure"="Marine life entanglement closure"),
         status=ifelse(status=="Out-of-season", NA, status),
         status=factor(status, levels=c("Season open", "Body condition delay",
                                        "Body condition/domoic acid delay", "Domoic acid delay",
                                        "Evisceration order", "Marine life entanglement closure")))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Limits
  # scale_y_continuous() +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="ODFW Commercial Dungeness crab closures") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "coral", "navy"), na.translate = F, drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_odfw_comm_dcrab_closures.png"),
       width=6.5, height=3, units="in", dpi=600)


