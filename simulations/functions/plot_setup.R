
# Plot scenario setup
plot_setup <- function(toxin_grid_df, stations, mgmt_zones){

  # Build management data frame
  mgmt_df <- tibble(type=c(rep("Sampling station", length(stations)),
                           rep("Management zones", length(mgmt_zones))),
                    lat_dd=c(stations, mgmt_zones))

  # Build matrix
  g <- ggplot(toxin_grid_df, mapping=aes(x=day, y=lat, fill=prop)) +
    # Plot raster
    geom_raster() +
    # Plot contours
    geom_contour(data=toxin_grid_df,
                 mapping=aes(x=day, y=lat, z=prop),
                 breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2, lwd=0.2) +
    # Labels
    labs(x="Day", y="Latitude (°N)") +
    # Stations/zones
    geom_hline(data=mgmt_df, mapping=aes(yintercept=lat_dd, linetype=type)) +
    # Legend
    scale_linetype(name="") +
    scale_fill_gradientn(name="Proportion above\naction threshhold",
                         na.value = "white",
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw()
  print(g)

  # Return grid
  return(g)

}
