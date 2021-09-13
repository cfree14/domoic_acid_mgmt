
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
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Parameters
ngrids <- 100
scenarios <- c("small", "medium", "large")


# Simulate data - diagonal
################################################################################

# Loop through scenarios
x <- "small"; y <- 1
data_diag <- purrr::map_df(scenarios, function(x){

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



# Simulate data - seeded
################################################################################

# Loop through scenarios
x <- "small"; y <- 1
data_seed <- purrr::map_df(scenarios, function(x){

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


# Merge graded/seeded simulations
################################################################################

# Merge grids
data <- data_diag %>%
  # Add seed
  left_join(data_seed, by=c("scenario", "iter", "lat", "day")) %>%
  # Calculate proportion
  mutate(prop=pmax(prop.x, prop.y))


# Export data
################################################################################

# Export data
saveRDS(data_diag, file.path(outputdir, "contamination_simulations.Rds"))
saveRDS(data, file.path(outputdir, "contamination_simulations_with_hotspots.Rds"))

