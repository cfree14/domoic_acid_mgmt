
# Plot scenario setup
plot_setup <- function(toxin_grid, stations, mgmt_zones){

  # Convert to raster
  toxin_df <- convert_raster(toxin_grid)

  # Build management data frame
  mgmt_df <- tibble(type=c(rep("Sampling station", length(stations)),
                           rep("Management zones", length(mgmt_zones))),
                    lat_dd=c(stations, mgmt_zones))

  # Build matrix
  g <- ggplot(toxin_df, mapping=aes(x=day, y=lat_dd, fill=p_over)) +
    geom_raster() +
    # Labels
    labs(x="Day", y="Latitude (°N)") +
    # Stations/zones
    geom_hline(data=mgmt_df, mapping=aes(yintercept=lat_dd, linetype=type)) +
    # Legend
   scale_linetype(name="") +
    scale_fill_gradient2(name="Proportion of crabs\nabove the action threshold", midpoint=0.5, low="navy", high="darkred") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw()
  print(g)

  # Return grid
  return(g)

}
