
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

# Parameters
niter <- 10
nstations <- seq(2, 20, 2)
key <- expand.grid(nstations=nstations,
                   iter=1:niter) %>%
  arrange(nstations, iter)

# Loop though number of stations
x <- 1
results <- purrr::map_df(1:nrow(key), function(x){

  # Extract parameters
  nstations <- key$nstations[x]
  iter <- key$iter[x]

  # Setup grid
  grid_temp <- set_grid(ymin=42, ymax=46, yres=0.05, ndays=180)

  # Setup management
  mgmt_info <- set_mgmt_even(nstations=nstations, grid=grid_temp, plot=F)
  stations <- mgmt_info$stations
  mgmt_zones <- mgmt_info$zones

  # Setup toxin grid
  toxin_grid <- simulate_toxin_grid(grid=grid_temp, type="diagonal", plot=F)
  # plot_setup(toxin_grid, stations, mgmt_zones)

  # Run scenario
  output <- simulate_mgmt(toxin_grid, stations, mgmt_zones, plot=F)

  # Quantify performance
  perf_stats <- quantify_performance(output)

  # Prepare results
  results_i <- perf_stats %>%
    mutate(nstations=nstations,
           iter=iter) %>%
    select(nstations, iter, everything())

  # Return
  results_i

})

# Format results for plotting
stats <- results %>%
  # Calculate iteration-level stats
  group_by(nstations, iter) %>%
  summarize(p_risk_missed=n[status_diff=="Open recklessly"]/(n[status_diff=="Open recklessly"] + n[status_diff=="Closed correctly"]),
            p_close_unneeded=n[status_diff=="Closed unnecessarily"] / (n[status_diff=="Closed unnecessarily"] + n[status_diff=="Closed correctly"])) %>%
  ungroup() %>%
  # Calculate station-level averages
  group_by(nstations) %>%
  summarize(p_risk_missed=mean(p_risk_missed),
            p_close_unneeded=mean(p_close_unneeded)) %>%
  ungroup()

# Plot results
g <- ggplot(stats, aes(x=p_risk_missed, y=p_close_unneeded, fill=nstations)) +
  geom_point(pch=21, size=4) +
  geom_point(x=0, y=0, color="black", size=4, pch=16) +
  # Axes
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), lim=c(0,NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), lim=c(0,NA)) +
  # Legend
  scale_fill_gradientn(name="Number of stations", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(x="Proportion of PH risk missed by closures", y="Proportion of fishery closures\nthat were unnecessary") +
  # Theme
  theme_bw()
g











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



