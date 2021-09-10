
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
codedir <- "simulations/functions"
outputdir <- "simulations/output"

# Source functions
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))


# Simulate data
################################################################################

# Parameters
ngrids <- 10
scenarios <- c("small", "medium", "large")

# Loop through scenarios
x <- "small"; y <- 1
diag_orig <- purrr::map_df(scenarios, function(x){

  # Loop through grids
  data <- purrr::map_df(1:ngrids, function(y){

    # Set params
    params <- set_sim_params(scenario=x, param_key=param_key)

    # Simulate grid
    grid <- simulate_toxin_grid_diag(prop_top = params$prop_top,
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


# Plot data
################################################################################

# Format data for plotting
diag <- diag_orig %>%
  # Format scenario
  mutate(scenario=recode_factor(scenario,
                                "small"="Small bloom",
                                "medium"="Medium bloom",
                                "large"="Large bloom")) %>%
  # Add date
  mutate(date=ymd("2020-12-01") + day - 1)

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

# Plot grid
g <- ggplot(diag, aes(x=date, y=lat, fill=prop)) +
  # Facet
  facet_grid(iter~scenario) +
  # Plot grid
  geom_raster() +
  # Plot contours
  geom_contour(data=diag,
               mapping=aes(x=date, y=lat, z=prop),
               breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2) +
  # Plot parameter limits
  geom_line(data=param_key_plot, aes(x=date, y=lat, group=position), inherit.aes = F) +
  # Labels
  labs(x="Day", y="Latitude (°N)") +
  scale_x_date(breaks=seq("2020-01-01" %>% ymd(), "2021-08-01" %>% ymd(), by="1 month"), date_labels = "%b") +
  # Legend
  scale_fill_gradientn(name="Proportion above\naction threshhold",
                       na.value = "white",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme(legend.position = "bottom")
g

