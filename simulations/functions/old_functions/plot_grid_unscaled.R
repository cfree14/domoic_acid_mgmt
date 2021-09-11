
# Plot  unscaled toxin grid
plot_grid_unscaled <- function(toxin_grid){

  # Convert to raster
  toxin_df <- convert_raster(toxin_grid)

  # Build matrix
  g <- ggplot(toxin_df, mapping=aes(x=day, y=lat_dd, fill=p_over)) +
    geom_raster() +
    # Labels
    labs(x="Day", y="Latitude (°N)") +
    # Legend
    # scale_fill_gradient2(name="Value", midpoint=0.5, low="navy", high="darkred") +
    scale_fill_gradientn(name="Value", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() +
    theme(legend.position = "bottom")
  print(g)

  # Return grid
  return(g)

}
