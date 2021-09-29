
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
outputdir <- "simulations/output"
codedir <- "simulations/functions"

# Read toxin grids
data <- readRDS(file.path(outputdir, "contamination_simulations_with_hotspots.Rds"))

# Source functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))


# Build data
################################################################################

# Toxin grids
toxin_grids <- data %>%
  # Reduce to example iteration
  filter(iter==14) %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1)

# Extract toxin grids
toxin_grid_sm <- toxin_grids %>%
  filter(scenario=="Small bloom")
toxin_grid_md <- toxin_grids %>%
  filter(scenario=="Medium bloom")
toxin_grid_lg <- toxin_grids %>%
  filter(scenario=="Large bloom")


# Format simulation parameters
################################################################################

# Extract lat span
lat_span_grad <- param_key %>%
  # Simplify
  select(size, span_lo, span_hi) %>%
  # Format
  rename(scenario=size) %>%
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Gather
  gather(key="end", value="lat", 2:ncol(.)) %>%
  # Format lat
  mutate(lat=46-lat)

# Extract lat span used
lat_span_used <- toxin_grids %>%
  filter(day==1) %>%
  arrange(scenario, desc(lat)) %>%
  group_by(scenario) %>%
  summarize(lat=max(lat[prop==0]))

# Extract last days (for gradient)
last_days_grad <- toxin_grids %>%
  # Unique
  select(scenario, last_day_top, last_day_bot) %>%
  unique() %>%
  # Gather
  gather(key="position", value="day", 2:3) %>%
  mutate(position=ifelse(grepl("top", position), "top", "bottom")) %>%
  # Add latitude
  mutate(lat=ifelse(position=="top", 46, 42)) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1)

# Format parameter key for plotting
param_key_plot <- param_key %>%
  # Simplify
  select(size, last_day_top_lo, last_day_top_hi, last_day_bot_lo, last_day_bot_hi) %>%
  # Recalculate last day bottom limits (convert from proportions to days)
  mutate(last_day_bot_lo=last_day_bot_lo*last_day_top_lo %>% ceiling(),
         last_day_bot_hi=last_day_bot_hi*last_day_top_hi %>% ceiling()) %>%
  # Gather
  gather(key="metric", value="value", 2:ncol(.)) %>%
  # Format
  mutate(position=ifelse(grepl("top", metric), "top", "bottom"),
         limit=ifelse(grepl("hi", metric), "high", "low"),
         lat=ifelse(position=="top", 46, 42)) %>%
  # Format scenario
  rename(scenario=size, day=value) %>%
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Add date
  mutate(date=ymd("2020-12-01") + day - 1) %>%
  # Arrange
  select(scenario, position, lat, limit, day, date)

# Format centroid bounding box
centroid_box <- param_key %>%
  # Simiplify
  select(size, center_x_lo, center_x_hi, center_y_lo, center_y_hi) %>%
  # Format scenario
  rename(scenario=size) %>%
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Add date
  mutate(center_x_lo_date=ymd("2020-12-01") + center_x_lo - 1,
         center_x_hi_date=ymd("2020-12-01") + center_x_hi - 1)

# Format ellipse span
ellipse_span <- param_key %>%
  # Add columns
  mutate(center_x_avg=(center_x_hi+center_x_lo)/2,
         center_y_avg=(center_y_hi+center_y_lo)/2,
         day_lo_small = center_x_avg - span_x_lo/2,
         day_hi_small = center_x_avg + span_x_lo/2,
         day_lo_large = center_x_avg - span_x_hi/2,
         day_hi_large = center_x_avg + span_x_hi/2,
         lat_lo_small = center_y_avg - span_y_lo/2,
         lat_hi_small = center_y_avg + span_y_lo/2,
         lat_lo_large = center_y_avg - span_y_hi/2,
         lat_hi_large = center_y_avg + span_y_hi/2) %>%
  # Simplify
  select(size, center_x_avg, center_y_avg, day_lo_small:lat_hi_large) %>%
  rename(scenario=size) %>%
  # Gather
  gather(key="metric", value="value", 4:ncol(.)) %>%
  # Add columns
  mutate(axis=ifelse(grepl("day", metric), "day", "latitude"),
         size=ifelse(grepl("large", metric), "Largest possible", "Smallest possible"),
         limit=ifelse(grepl("lo", metric), "low", "high")) %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Spread
  select(-metric) %>%
  spread(key="axis", value="value") %>%
  mutate(date=ymd("2020-12-01") + day - 1,
         center_x_avg_date=ymd("2020-12-01") + center_x_avg - 1)


# Run simulations
################################################################################

# Small scenario
output_sm <- simulate_mgmt(toxin_grid_df = toxin_grid_sm,
                           nstations=4,
                           repeat_interval = 0,
                           perfect = T,
                           plot = T)

# Medium scenario
output_md <- simulate_mgmt(toxin_grid_df = toxin_grid_md,
                           nstations=8,
                           repeat_interval = 6,
                           perfect = T,
                           plot = T)

# Large scenario
output_lg <- simulate_mgmt(toxin_grid_df = toxin_grid_lg,
                           nstations=12,
                           repeat_interval = 4,
                           perfect = T,
                           plot = T)

# Extract survey results
survey_results_sm <- output_sm$survey_results %>%
  mutate(scenario="Small bloom")
survey_results_md <- output_md$survey_results %>%
  mutate(scenario="Medium bloom")
survey_results_lg <- output_lg$survey_results %>%
  mutate(scenario="Large bloom")

# Merge and format survey results
survey_results <- bind_rows(survey_results_sm, survey_results_md, survey_results_lg) %>%
  # Arrange
  select(scenario, everything()) %>%
  # Format result
  mutate(status_obs=stringr::str_to_sentence(status_obs)) %>%
  # Format scenario
  mutate(scenario=factor(scenario,
                         levels=c("Small bloom", "Medium bloom", "Large bloom"))) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1)

# Extract management results
mgmt_grid_sm <- output_sm$mgmt_grid %>%
  mutate(scenario="Small bloom")
mgmt_grid_md <- output_md$mgmt_grid %>%
  mutate(scenario="Medium bloom")
mgmt_grid_lg <- output_lg$mgmt_grid %>%
  mutate(scenario="Large bloom")

# Merge and format management grid
mgmt_grid <- bind_rows(mgmt_grid_sm, mgmt_grid_md, mgmt_grid_lg) %>%
  # Arrange
  select(scenario, everything()) %>%
  # Format scenario
  mutate(scenario=factor(scenario,
                         levels=c("Small bloom", "Medium bloom", "Large bloom"))) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1) %>%
  # Factor status
  mutate(status_diff=factor(status_diff,
                           levels=c("Closed correctly",
                                    "Closed unnecessarily",
                                    "Open correctly",
                                    "Open riskily")))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=9),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position="right",
                    legend.key.size = unit(0.5, "cm"))

# Plot toxin grids
g1 <- ggplot(toxin_grids, mapping=aes(x=date, y=lat, fill=prop)) +
  facet_wrap(~scenario) +
  # Raster
  geom_raster() +
  # Plot contours
  geom_contour(data=toxin_grids,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Plot early-season parameter limits and selection
  geom_line(data=param_key_plot, aes(x=date, y=lat, group=position),
            color="grey40", inherit.aes = F) +
  geom_point(data=last_days_grad, aes(x=date, y=lat), inherit.aes = F, size=0.8) +
  geom_line(data=lat_span_grad, aes(x=ymd("2020-12-01")-7, y=lat),
            color="grey40", inherit.aes = F) +
  geom_point(data=lat_span_used, aes(x=ymd("2020-12-01")-7, y=lat), inherit.aes = F, size=0.8) +
  # Plot mid-season bloom centroid box
  geom_rect(data=centroid_box, mapping=aes(xmin=center_x_lo_date,
                                           xmax=center_x_hi_date,
                                           ymin=center_y_lo,
                                           ymax=center_y_hi), inherit.aes = F, color="grey50", fill=NA, linetype="dotted") +
  # Plot mid-season bloom span
  geom_line(data=ellipse_span, mapping=aes(x=date, y=center_y_avg, color=size, size=size), inherit.aes=F, show.legend = F) +
  geom_line(data=ellipse_span, mapping=aes(x=center_x_avg_date, y=latitude, color=size, size=size), inherit.aes=F, show.legend = F) +
  # Label
  labs(x="", y="Latitude (°N)",
       title="Toxin contamination") +
  # Axes
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legend
  scale_size_manual(name="Bloom size", values=c(0.5, 1)) +
  scale_color_manual(name="Bloom size", values=c("red", "black")) +
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g1

# Plot monitoring
g2 <- ggplot(toxin_grids, mapping=aes(x=date, y=lat, fill=prop)) +
  facet_wrap(~scenario) +
  # Raster
  geom_raster(show.legend = F) +
  # Plot contours
  geom_contour(data=toxin_grids,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Plot survey results
  geom_point(data=survey_results, aes(x=date, y=lat, shape=status_obs, size=status_obs), inherit.aes = F) +
  # Label
  labs(x="", y="Latitude (°N)",
       title="Toxin monitoring results") +
  # Axes
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legends
  scale_shape_manual(name="Survey result", values=c(21, 16)) +
  scale_size_manual(name="Survey result", values=c(0.9, 1.2), guide="none") +
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g2

# Plot mangament
g3 <- ggplot(mgmt_grid, aes(x=date, y=lat, fill=status_diff)) +
  facet_wrap(~scenario) +
  # Raster
  geom_raster() +
  # Plot contours
  geom_contour(data=toxin_grids,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)),
               color="black", alpha=0.2, lwd=0.2, inherit.aes = F) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Plot survey results
  geom_point(data=survey_results, aes(x=date, y=lat, shape=status_obs, size=status_obs), inherit.aes = F) +
  # Label
  labs(x="", y="Latitude (°N)",
       title="Toxin management performance") +
  # Axes
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Point legend
  scale_shape_manual(name="Survey result", values=c(21, 16),  guide="none") +
  scale_size_manual(name="Survey result", values=c(0.9, 1.2), guide="none") +
  # Fill legend
  scale_fill_manual(name="Management\nperformance", drop=F,
                    values=c("#F8766D",
                             alpha("#F8766D", 0.5),
                             "#00BFC4",
                             alpha("#00BFC4", 0.5))) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3)

# Export
ggsave(g, filename=file.path(plotdir, "Fig9_simulation_workflow_example.png"),
       width=6.5, height=5.5, units="in", dpi=600)


