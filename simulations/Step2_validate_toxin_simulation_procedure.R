
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"
tabledir <- "tables"
codedir <- "simulations/functions"
outputdir <- "simulations/output"

# Source functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Parameters
ngrids <- 10
scenarios <- c("small", "medium", "large")


# Simulate data - diagonal
################################################################################

# Loop through scenarios
x <- "small"; y <- 1
diag_orig <- purrr::map_df(scenarios, function(x){

  # Loop through grids
  data <- purrr::map_df(1:ngrids, function(y){

    # Set params
    params <- set_sim_params(scenario=x, param_key=param_key)

    # Simulate grid
    grid <- simulate_toxin_grid_diag(span = params$span,
                                     prop_top = params$prop_top,
                                     prop_bot = params$prop_bot,
                                     last_day_top = params$last_day_top,
                                     last_day_bot = params$last_day_bot,
                                     plot=F)

    # Format grid
    grid_out <- grid %>%
      mutate(scenario=x,
             iter=y,
             prop_top = params$prop_top,
             prop_bot = params$prop_bot,
             last_day_top = params$last_day_top,
             last_day_bot = params$last_day_bot) %>%
      select(scenario:last_day_bot, everything())

  })

})


# Plot data - diagonal
################################################################################

# Format data for plotting
diag <- diag_orig %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
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
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
  # Add date
  mutate(date=ymd("2020-12-01") + day - 1) %>%
  # Arrange
  select(scenario, position, lat, limit, day, date)

# Extract lat span
lat_span_grad <- param_key %>%
  # Simplify
  select(size, span_lo, span_hi) %>%
  # Format
  rename(scenario=size) %>%
  mutate(scenario=recode_factor(scenario,
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
  # Gather
  gather(key="end", value="lat", 2:ncol(.)) %>%
  # Format lat
  mutate(lat=46-lat)

# Extract lat span used
lat_span_used <- diag_orig %>%
  filter(day==1) %>%
  arrange(scenario, desc(lat)) %>%
  group_by(scenario, iter) %>%
  summarize(lat=max(lat[prop==0])) %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                              "small"="Small event",
                              "medium"="Medium event",
                              "large"="Large event"))

# Extract last days (for gradient)
last_days_grad <- diag_orig %>%
  # Unique
  select(scenario, iter, last_day_top, last_day_bot) %>%
  unique() %>%
  # Gather
  gather(key="position", value="day", 3:4) %>%
  mutate(position=ifelse(grepl("top", position), "top", "bottom")) %>%
  # Add latitude
  mutate(lat=ifelse(position=="top", 46, 42)) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1) %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event"))

# Plot grid
g1 <- ggplot(diag, aes(x=date, y=lat, fill=prop)) +
  # Facet
  facet_grid(iter~scenario) +
  # Plot grid
  geom_raster() +
  # Plot contours
  geom_contour(data=diag,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Plot parameter limits
  geom_line(data=param_key_plot, aes(x=date, y=lat, group=position), inherit.aes = F) +
  geom_point(data=last_days_grad, aes(x=date, y=lat), inherit.aes = F, size=0.8) +
  # Plot lat span
  geom_line(data=lat_span_grad, aes(x=ymd("2020-12-01")-7, y=lat),
            color="grey40", inherit.aes = F) +
  geom_point(data=lat_span_used, aes(x=ymd("2020-12-01")-7, y=lat), inherit.aes = F, size=0.4) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legend
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigS7_toxin_grid_examples_diag.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Plot data - seeded
################################################################################

# Loop through scenarios
x <- "small"; y <- 1
seed_orig <- purrr::map_df(scenarios, function(x){

  # Loop through grids
  data <- purrr::map_df(1:ngrids, function(y){

    # Set params
    params <- set_sim_params(scenario=x, param_key=param_key)

    # Simulate grid
    grid <- simulate_toxin_grid_seed(center_val = params$center_prop,
                                     center_day = params$center_x,
                                     center_lat = params$center_y,
                                     span_days = params$span_x,
                                     span_lat = params$span_y,
                                     plot=F)

    # Format grid
    grid_out <- grid %>%
      mutate(scenario=x,
             iter=y,
             center_val = params$center_prop,
             center_day = params$center_x,
             center_lat = params$center_y,
             span_days = params$span_x,
             span_lat = params$span_y) %>%
      select(scenario:span_lat, everything())

  })

})


# Plot data - diagonal
################################################################################

# Format data for plotting
seed <- seed_orig %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
  # Add date
  mutate(date=ymd("2020-12-01") - 7 + day - 1)

# Format centroid bounding box
centroid_box <- param_key %>%
  # Simiplify
  select(size, center_x_lo, center_x_hi, center_y_lo, center_y_hi) %>%
  # Format scenario
  rename(scenario=size) %>%
  mutate(scenario=recode_factor(scenario,
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
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
                                "small"="Small event",
                                "medium"="Medium event",
                                "large"="Large event")) %>%
  # Spread
  select(-metric) %>%
  spread(key="axis", value="value") %>%
  mutate(date=ymd("2020-12-01") + day - 1,
         center_x_avg_date=ymd("2020-12-01") + center_x_avg - 1)

# Plot grid
g2 <- ggplot(seed, aes(x=date, y=lat, fill=prop)) +
  # Facet
  facet_grid(iter~scenario) +
  # Plot grid
  geom_raster() +
  # Plot contours
  geom_contour(data=seed,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Reference
  geom_rect(data=centroid_box, mapping=aes(xmin=center_x_lo_date,
                                           xmax=center_x_hi_date,
                                           ymin=center_y_lo,
                                           ymax=center_y_hi), inherit.aes = F, color="grey50", fill=NA, linetype="dotted") +
  # Plot large event
  geom_line(data=ellipse_span, mapping=aes(x=date, y=center_y_avg, color=size), inherit.aes=F) +
  geom_line(data=ellipse_span, mapping=aes(x=center_x_avg_date, y=latitude, color=size), inherit.aes=F) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legend
  scale_color_manual(name="Event\nsize", values=c("red", "black")) +
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order = 1)) +
  # Theme
  theme_bw() + my_theme
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "FigS8_toxin_grid_examples_seed.png"),
       width=6.5, height=6.5, units="in", dpi=600)



# Merge results
################################################################################

# Merge grids
data <- diag %>%
  # Add seed
  left_join(seed, by=c("scenario", "iter", "lat", "day", "date")) %>%
  # Calculate proportion
  mutate(prop=pmax(prop.x, prop.y))


# Plot grid
g3 <- ggplot(data, aes(x=date, y=lat, fill=prop)) +
  # Facet
  facet_grid(iter~scenario) +
  # Plot grid
  geom_raster() +
  # Plot contours
  geom_contour(data=data,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
  # Plot season opener
  geom_vline(xintercept=ymd("2020-12-01"), linetype="dotted", lwd=0.4) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legend
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g3

# Export figure
ggsave(g3, filename=file.path(plotdir, "FigS9_toxin_grid_examples_merged.png"),
       width=6.5, height=6.5, units="in", dpi=600)



