

# Clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
outputdir <- "simulations/output"
codedir <- "simulations/functions"

# Read data
data <- readRDS(file=file.path(outputdir, "simulation_output_graded_perfect.Rds"))


# Build data
################################################################################

# Mins if using log-scale
xmin <- 0.00005
ymin <- 0.001

# Format results for plotting
stats <- data %>%
  # Complete
  complete(nstations, toxin_scenario, toxin_scenario_iter, status_diff, fill=list(n=0, prop=0)) %>%
  # Calculate iteration-level stats
  group_by(nstations, toxin_scenario, toxin_scenario_iter) %>%
  summarize(p_risk_missed=n[status_diff=="Open riskily"] / (n[status_diff=="Open riskily"] + n[status_diff=="Closed correctly"]),
            p_close_unneeded=n[status_diff=="Closed unnecessarily"] /sum(n)) %>%
  # p_close_unneeded=n[status_diff=="Closed unnecessarily"] / (n[status_diff=="Closed unnecessarily"] + n[status_diff=="Closed correctly"])) %>%
  ungroup() %>%
  # Calculate station-level averages
  group_by(nstations, toxin_scenario) %>%
  summarize(p_risk_missed_avg=mean(p_risk_missed),
            p_risk_missed_med=median(p_risk_missed),
            p_risk_missed_min=min(p_risk_missed),
            p_risk_missed_max=max(p_risk_missed),
            p_close_unneeded_avg=mean(p_close_unneeded),
            p_close_unneeded_med=median(p_close_unneeded),
            p_close_unneeded_min=min(p_close_unneeded),
            p_close_unneeded_max=max(p_close_unneeded)) %>%
  ungroup() %>%
  # Calculate distance to origin
  group_by(toxin_scenario) %>%
  mutate(dist=sqrt(p_risk_missed_avg^2 + p_close_unneeded_avg^2),
         dist_rank=rank(dist)) %>%
  ungroup() %>%
  # Format bloom names
  mutate(toxin_scenario=recode_factor(toxin_scenario,
                                      "small"="Small bloom",
                                      "medium"="Medium bloom",
                                      "large"="Large bloom")) %>%
  # Filter for simplicity
  # filter(nstations %in% c(4, 5, 6, 8, 10, 12)) %>%
  # Format if using log-scale
  mutate(p_risk_missed_min=ifelse(p_risk_missed_min==0, xmin, p_risk_missed_min),
         p_close_unneeded_min=ifelse(p_close_unneeded_min==0, ymin, p_close_unneeded_min))


# OR 5 to 12 line segments
or_lines <- stats %>%
  # Classify
  mutate(season=ifelse(nstations==5, "2015-16", NA),
         season=ifelse(nstations==12, "2017-18", season)) %>%
  # Filter
  filter(!is.na(season))

# Stats for manuscript
or_stats <- or_lines %>%
  select(toxin_scenario, nstations, p_risk_missed_avg, p_close_unneeded_avg) %>%
  group_by(toxin_scenario) %>%
  summarize(risk_prevented=(p_risk_missed_avg[nstations==12] - p_risk_missed_avg[nstations==5]) *100,
            closures_prevented=(p_close_unneeded_avg[nstations==12]-p_close_unneeded_avg[nstations==5])*100)
mean(or_stats$risk_prevented)
mean(or_stats$closures_prevented)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot results
g <- ggplot(stats, aes(x=p_risk_missed_avg, y=p_close_unneeded_avg, fill=nstations)) +
  facet_wrap(~toxin_scenario, nrow=1) +
  # Ranges
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_min,
                                       xend=p_risk_missed_max,
                                       y=p_close_unneeded_avg,
                                       yend=p_close_unneeded_avg,
                                       color=nstations), alpha=0.5) +
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_avg,
                                       xend=p_risk_missed_avg,
                                       y=p_close_unneeded_min,
                                       yend=p_close_unneeded_max,
                                       color=nstations), alpha=0.5) +
  # OR lines
  geom_line(data=or_lines, mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg),
            inherit.aes = F, color="black", lwd=0.5) +
  # Means
  geom_point(pch=21, size=3.5, stroke=0.1) +
  geom_text(data=stats,
            mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg, label=nstations),
            size=2) +
  # Reference
  # geom_point(x=0, y=0, color="black", size=4, pch=16) +
  # Axes (not log-scale)
  # scale_x_continuous(labels = scales::percent_format(accuracy = 0.5)) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 0.5)) +
  # Axes (log-scale)
  scale_x_continuous(trans="log2",
                     breaks=c(0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.10, 0.20, 0.50),
                     labels=c("0.01%", "0.02%", "0.05%", "0.1%", "0.2%", "0.5%", "1%", "2%", "5%", "10%", "20%", "50%"),
                     lim=c(xmin, NA)) +
  scale_y_continuous(trans="log2",
                     breaks=c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.10),
                     labels=c("0.1%", "0.2%", "0.5%", "1%", "2%", "5%", "10%"),
                     lim=c(ymin, NA)) +
  # Legend
  scale_color_gradientn(name="Number of\nsites", colors=RColorBrewer::brewer.pal(9, "Greens")[3:9], guide="none") +
  scale_fill_gradientn(name="Number of\nsites", colors=RColorBrewer::brewer.pal(9, "Greens")[1:7]) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(x="Proportion of public health risk\noverlooked by closures",
       y="Proportion of fishing season\nclosed unnecessarily") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.94,0.3),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export data
ggsave(g, filename=file.path(plotdir, "Fig10_simulation_results_gradient_only.png"),
       width=6.5, height=2.75, units="in", dpi=600)





