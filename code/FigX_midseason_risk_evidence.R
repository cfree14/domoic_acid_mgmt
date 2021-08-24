

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(lubridate)
library(tidyverse)

# Directories
outdir <- "data/california/pier_sampling/data"
plotdir <- "figures"

# Read data
pn_orig <- readRDS(file.path(outdir, "2014_2021_pn_density_by_pier_for_plotting.Rds"))
pda_orig <- readRDS(file.path(outdir, "2014_2021_pda_density_by_pier_for_plotting.Rds"))


# Format data
################################################################################

# Format PN data
pn <- pn_orig %>%
  filter(location=="Monterey Wharf") %>%
  mutate(pn_group=factor(pn_group, levels=c("Seriata", "Delicatissima")))

# Format PN data
pda <- pda_orig %>%
  filter(location %in% c("Monterey Wharf"))

# Season key
season_key <- tibble(year1=2013:2020,
                     year2=2014:2021) %>%
  mutate(season=paste(year1, year2-2000, sep="-"),
         open_date=paste0(year1, "-11-15"),
         close_date=paste0(year2, "-06-30")) %>%
  # Recode 2014 season date
  mutate(open_date=ifelse(year1==2013, "2014-01-01", open_date)) %>%
  # Convert to date
  mutate(open_date=ymd(open_date),
         close_date=ymd(close_date))


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
                   legend.title=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


# Plot PN data
# Clarissa's bloom threshold is 10^4 cells / L
ymax <- max(pn$cells_l_max, na.rm=T) + 1
g1 <- ggplot() +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date), ymin=-10, ymax=ymax, fill="grey90") +
  # Lines
  geom_line(data=pn, mapping=aes(x=date_std, y=cells_l_max+1, color=pn_group), lwd=0.4) +
  # Bloom threshhold reference line
  geom_hline(yintercept=c(10^4), linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Cell\ndensity (cells/L)", tag="A",
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
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Plot pDA data
# Clarissa's threshold of evevated pDA is 0.5 ug/L = 0.5 ng/ml
# 0.5 ug/L * (1 L / 1000 mL) * (1 g / 10^6 ug) * (10^9 ng / 1 g)
0.5  / 1000 / 10^6 * 10^9
ymax <- max(pda$pda_ng_ml_max, na.rm=T) + 0.00001
g2 <- ggplot() +
  # Seasons
  geom_rect(data=season_key, mapping=aes(xmin=open_date, xmax=close_date), ymin=-10, ymax=ymax, fill="grey90") +
  # Lines
  geom_line(data=pda, mapping=aes(x=date_std, y=pda_ng_ml_max+0.00001), lwd=0.4) +
  # Elevated pDA reference line
  geom_hline(yintercept=0.5, linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Particulate\ndomoic acid (ng/ml)", tag="B",
       title="Historical observations of particulate domoic acid at Monterey Wharf") +
  # Scales
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(trans="log10",
                     breaks=10^c(1:-5),
                     # labels=paste0("10^", 1:-5),
                     labels=parse(text=paste0("10^", 1:-5))) +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_midseason_risk_evidence.png"),
       width=6.5, height=4, units="in", dpi=600)

