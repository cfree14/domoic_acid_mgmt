
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

# Read toxin grids
data1 <- readRDS(file.path(outputdir, "contamination_simulations.Rds"))
data2 <- readRDS(file.path(outputdir, "contamination_simulations_with_hotspots.Rds"))

# Source functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))


# Simulate monitoring and management
################################################################################

# Parameters
nstations <- seq(2, 12, 2)
toxin_scenarios <- sort(unique(data1$scenario))
toxin_scenarios_niter <- max(data1$iter)

# Build scenario gird
scenario_key <- expand.grid(nstations=nstations,
                            toxin_scenario=toxin_scenarios,
                            toxin_scenario_iter=1:toxin_scenarios_niter)

# Loop through scenarios
x <- 3
results <- purrr::map_df(1:nrow(scenario_key), function(x){

  # Extract parameters
  nstations_do <- scenario_key$nstations[x]
  toxin_scenario <- scenario_key$toxin_scenario[x]
  toxin_scenario_iter <- scenario_key$toxin_scenario_iter[x]

  # Extract toxin grid
  toxin_grid_df <- data1 %>%
    filter(scenario==toxin_scenario & iter==toxin_scenario_iter)

  # Extract grid parameters
  ymin <- min(toxin_grid_df$lat)
  ymax <- max(toxin_grid_df$lat)
  ndays <- max(toxin_grid_df$day)

  # Setup management
  mgmt_info <- set_mgmt_even(nstations=nstations_do, ymin=ymin, ymax=ymax, ndays=ndays, plot=F)
  stations <- mgmt_info$stations
  mgmt_zones <- mgmt_info$zones

  # Plot check
  # plot_setup(toxin_grid_df, stations, mgmt_zones)

  # Run scenario
  output <- simulate_mgmt(toxin_grid_df, stations, mgmt_zones, perfect=T, plot=F)

  # Quantify performance
  perf_stats <- quantify_performance(output)

  # Prepare results
  results_i <- perf_stats %>%
    mutate(nstations=nstations_do,
           toxin_scenario=toxin_scenario,
           toxin_scenario_iter=toxin_scenario_iter) %>%
    select(nstations, toxin_scenario, toxin_scenario_iter, everything())

  # Return
  results_i


})

# Export
saveRDS(results, file=file.path(outputdir, "simulation_output_graded_perfect.Rds"))


# Simulate monitoring and management
################################################################################

# Format results for plotting
stats <- results %>%
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
            p_risk_missed_min=min(p_risk_missed),
            p_risk_missed_max=max(p_risk_missed),
            p_close_unneeded_avg=mean(p_close_unneeded),
            p_close_unneeded_min=min(p_close_unneeded),
            p_close_unneeded_max=max(p_close_unneeded)) %>%
  ungroup()

# Plot results
g <- ggplot(stats, aes(x=p_risk_missed_avg, y=p_close_unneeded_avg, fill=nstations)) +
  facet_wrap(~toxin_scenario, nrow=1, scales="free") +
  # Ranges
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_min,
                                       xend=p_risk_missed_max,
                                       y=p_close_unneeded_avg,
                                       yend=p_close_unneeded_avg,
                                       color=nstations), show.legend = F) +
  geom_segment(data=stats, mapping=aes(x=p_risk_missed_avg,
                                       xend=p_risk_missed_avg,
                                       y=p_close_unneeded_min,
                                       yend=p_close_unneeded_max,
                                       color=nstations), show.legend = F) +
  # Means
  geom_point(pch=21, size=4) +
  # Reference
  geom_point(x=0, y=0, color="black", size=4, pch=16) +
  # Axes
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), lim=c(0,NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.5), lim=c(0,NA)) +
  # Legend
  scale_color_gradientn(name="Number of stations", colors=RColorBrewer::brewer.pal(9, "Purples")) +
  scale_fill_gradientn(name="Number of stations", colors=RColorBrewer::brewer.pal(9, "Purples")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Labels
  labs(x="Proportion of PH risk missed by closures", y="Proportion of fishery closures\nthat were unnecessary") +
  # Theme
  theme_bw() +
  theme(legend.position="bottom")
g







