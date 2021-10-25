

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
data <- readRDS(file=file.path(outputdir, "simulation_output_seeded_perfect.Rds"))


# Build data
################################################################################

# Format results for plotting
stats <- data %>%
  # Complete
  complete(nstations, interval_wk, toxin_scenario, toxin_scenario_iter, status_diff, fill=list(n=0, prop=0)) %>%
  # Calculate iteration-level stats
  group_by(nstations, interval_wk, toxin_scenario, toxin_scenario_iter) %>%
  summarize(p_risk_missed=n[status_diff=="Open riskily"] / (n[status_diff=="Open riskily"] + n[status_diff=="Closed correctly"]),
            p_close_unneeded=n[status_diff=="Closed unnecessarily"] /sum(n)) %>%
  # p_close_unneeded=n[status_diff=="Closed unnecessarily"] / (n[status_diff=="Closed unnecessarily"] + n[status_diff=="Closed correctly"])) %>%
  ungroup() %>%
  # Calculate station-level averages
  group_by(nstations, interval_wk, toxin_scenario) %>%
  summarize(p_risk_missed_avg=mean(p_risk_missed),
            p_risk_missed_min=min(p_risk_missed),
            p_risk_missed_max=max(p_risk_missed),
            p_close_unneeded_avg=mean(p_close_unneeded),
            p_close_unneeded_min=min(p_close_unneeded),
            p_close_unneeded_max=max(p_close_unneeded)) %>%
  ungroup() %>%
  # Calculate distance to origin
  group_by(toxin_scenario) %>%
  mutate(dist=sqrt(p_risk_missed_avg^2 + p_close_unneeded_avg^2),
         dist_rank=rank(dist)) %>%
  ungroup() %>%
  # Format event names
  mutate(toxin_scenario=recode_factor(toxin_scenario,
                                      "small"="Small event",
                                      "medium"="Medium event",
                                      "large"="Large event")) %>%
  # Format interval
  mutate(interval_wk=interval_wk %>% as.character() %>%  paste(., "weeks"),
         interval_wk=recode(interval_wk,
                            "1 weeks"="1 week",
                            '0 weeks'="No repeat"),
         interval_wk=factor(interval_wk,
                            levels=c("1 week", paste(c(2,3,4,6), "weeks"), "No repeat"))) %>%
  # Arrange
  arrange(toxin_scenario, nstations, interval_wk) %>%
  # Reduce to scenarios of interest
  filter(nstations %in% c(2, 5, 8, 12))

# OR 5 to 12 line segments
or_lines <- stats %>%
  # Classify
  mutate(season=ifelse(nstations==5 & interval_wk=="No repeat", "2015-16", NA),
         season=ifelse(nstations==12 & interval_wk=='4 weeks', "2017-18", season)) %>%
  # Filter
  filter(!is.na(season))


# Stats for MS
################################################################################

# Large event, 12 station
stats %>%
  filter(toxin_scenario=="Large event" & nstations==12)


# Small event, 12 station
stats %>%
  filter(toxin_scenario=="Small event" & nstations==12)

# OR stats
or_stats <- or_lines %>%
  select(toxin_scenario, season, p_risk_missed_avg, p_close_unneeded_avg) %>%
  group_by(toxin_scenario) %>%
  summarize(risk_prevented=(p_risk_missed_avg[season=="2017-18"] - p_risk_missed_avg[season=="2015-16"]) *100,
            closures_prevented=(p_close_unneeded_avg[season=="2017-18"]-p_close_unneeded_avg[season=="2015-16"])*100)
mean(or_stats$risk_prevented)
mean(or_stats$closures_prevented)

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot results
g <- ggplot(stats, aes(x=p_risk_missed_avg,
                       y=p_close_unneeded_avg,
                       fill=nstations,
                       size=interval_wk)) +
  # Facet
  facet_wrap(~toxin_scenario, nrow=1) +
  # Ranges
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_min,
                                       xend=p_risk_missed_max,
                                       y=p_close_unneeded_avg,
                                       yend=p_close_unneeded_avg,
                                       color=nstations),
               inherit.aes = F, show.legend = F, lwd=0.3, alpha=0.5) +
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_avg,
                                       xend=p_risk_missed_avg,
                                       y=p_close_unneeded_min,
                                       yend=p_close_unneeded_max,
                                       color=nstations),
               inherit.aes = F, show.legend = F, lwd=0.3, alpha=0.5) +
  # Lines
  geom_path(data=stats, mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg,
                                    color=nstations, group=nstations), inherit.aes = F) +
  # Points
  geom_point(pch=21, stroke=0.1, color="black") +
  # OR lines
  geom_line(data=or_lines, mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg),
            inherit.aes = F, color="black", lwd=0.8) +
  geom_point(data=or_lines, mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg),
             inherit.aes = F, color="black", fill="white", pch=21) +
  geom_text(data=or_lines, mapping=aes(x=p_risk_missed_avg, y=p_close_unneeded_avg, label=season),
            inherit.aes = F, color="black", hjust=0, nudge_x = 0.05,  size=2.5) +
  # Axes
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), lim=c(0,NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), lim=c(0,NA)) +
  # Labels
  labs(x="Proportion of public health risk\noverlooked by closures",
       y="Proportion of fishing season\nclosed unnecessarily") +
  # Legend
  scale_size_ordinal(name="Resample\ninterval", range=c(0.5, 3)) +
  scale_color_gradientn(name="Number of\nsites", colors=RColorBrewer::brewer.pal(9, "Greens")[4:9], guide="none") +
  scale_fill_gradientn(name="Number of\nsites", colors=RColorBrewer::brewer.pal(9, "Greens")[2:7]) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "Fig11_simulation_results_seeded.png"),
       width=6.5, height=2.5, units="in", dpi=600)





