
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(NLMR)
library(landscapetools)
library(raster)
library(tidyverse)

# Directories
codedir <- "simulations/functions"

# Read functions
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))


# Run some simulation
################################################################################

# Setup scenario
grid_temp <- set_grid(ymin=42, ymax=46, yres=0.05, ndays=180)
mgmt_info <- set_mgmt_even(nstations=10, grid=grid_temp)
stations <- mgmt_info$stations
mgmt_zones <- mgmt_info$zones
toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="auto")
plot_setup(toxin_grid, stations, mgmt_zones)

# Run scenario
results <- simulate_mgmt(toxin_grid, stations, mgmt_zones)

# Plot results of scenario





# toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="uniform")
# toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="auto")
# toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="bulge")

