
# Plot toxin grid
plot_grid <- function(toxin_grid){

  # Convert to raster
  toxin_df <- convert_raster(toxin_grid)

  # Build matrix
  g <- ggplot(toxin_df, mapping=aes(x=day, y=lat_dd, fill=p_over)) +
    geom_raster() +
    # Labels
    labs(x="Day", y="Latitude (°N)") +
    # Legend
    scale_fill_gradient2(name="Proportion of crabs\nabove the action threshold", midpoint=0.5, low="navy", high="darkred") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() +
    theme(legend.position = "bottom")
  print(g)

  # Return grid
  return(g)

}
