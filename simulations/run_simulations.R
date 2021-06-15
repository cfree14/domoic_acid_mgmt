
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
plotdir <- "simulations/figures"
codedir <- "simulations/functions"

# Read functions
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))


# Run some simulation
################################################################################

# Setup scenario
grid_temp <- set_grid(ymin=42, ymax=46, yres=0.05, ndays=180)
mgmt_info <- set_mgmt_even(nstations=12, grid=grid_temp)
stations <- mgmt_info$stations
mgmt_zones <- mgmt_info$zones
toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="auto")
plot_setup(toxin_grid, stations, mgmt_zones)

# Run scenario
results <- simulate_mgmt(toxin_grid, stations, mgmt_zones)

# Plot results of scenario
toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="diagonal")
toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="auto")
toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="bulge")



# Figure
################################################################################

# Grid
grid_temp <- set_grid(ymin=42, ymax=46, yres=0.05, ndays=180)

# 12 mgmt zones
mgmt_info12 <- set_mgmt_even(nstations=12, grid=grid_temp)
stations12 <- mgmt_info12$stations
mgmt_zones12 <- mgmt_info12$zones

# 5 mgmt zones
mgmt_info5 <- set_mgmt_even(nstations=5, grid=grid_temp)
stations5 <- mgmt_info5$stations
mgmt_zones5 <- mgmt_info5$zones

# Grids
toxin_grid3 <- simulate_toxin_grid(grid=grid_temp, type="auto")
toxin_grid2 <- simulate_toxin_grid(grid=grid_temp, type="diagonal")
toxin_grid1 <- simulate_toxin_grid(grid=grid_temp, type="bulge")


# 12 zones
g1 <- plot_setup(toxin_grid2, stations12, mgmt_zones12) + theme(legend.position = "none")
g2 <- plot_setup(toxin_grid1, stations12, mgmt_zones12) + theme(legend.position = "none")
g3 <- plot_setup(toxin_grid3, stations12, mgmt_zones12) + theme(legend.position = "none")

# 5 zones
g4 <- plot_setup(toxin_grid2, stations5, mgmt_zones5) + theme(legend.position = "none")
g5 <- plot_setup(toxin_grid1, stations5, mgmt_zones5) + theme(legend.position = "none")
g6 <- plot_setup(toxin_grid3, stations5, mgmt_zones5) + theme(legend.position = "none")

# Merge plots
g <- gridExtra::grid.arrange(g1, g4,
                             g2, g5,
                             g3, g6, ncol=2)

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_example_scenarios.png"),
       width=6.5, height=5, units="in", dpi=600)



