
# Plot surveys and management decigions
plot_surveys_all <- function(toxin_grid_df, survey_results_full){

  # Plot grid and sampling
  g <- ggplot(toxin_grid_df, aes(x=day, y=lat, fill=prop)) +
    geom_raster() +
    # Surveys
    geom_point(data=survey_results_full, mapping=aes(x=day, y=lat, shape=status_obs), inherit.aes = F) +
    # Season opener
    geom_vline(xintercept=7, linetype="dotted") +
    # Labels
    labs(x="Day", y="Latitude (°N)", title="All surveys (unpared)") +
    # Legend
    scale_fill_gradientn(name="True proportion of crabs\nabove action threshold",
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                         lim=c(0,1)) +
    scale_shape_manual(name="Estimated status", values=c(21, 16)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw()
  g

  # Return
  print(g)
  return(g)

}
