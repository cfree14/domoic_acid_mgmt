
# Plot surveys and management decigions
plot_surveys_mgmt <- function(toxin_grid_df, survey_results, mgmt_grid_df){

  # Base theme
  base_theme <- theme(axis.text=element_text(size=7),
                      axis.title=element_text(size=8),
                      plot.title=element_text(size=9),
                      legend.text=element_text(size=7),
                      legend.title=element_text(size=8),
                      # Gridlines
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      # Legend
                      legend.position="right",
                      legend.key.size = unit(0.3, "cm"))

  # Plot monitoring
  g1 <- ggplot(toxin_grid_df, aes(x=day, y=lat, fill=prop)) +
    geom_raster() +
    # Surveys
    geom_point(data=survey_results, mapping=aes(x=day, y=lat, shape=status_obs),
               inherit.aes = F) +
    # Season opener
    geom_vline(xintercept=7, linetype="dotted") +
    # Labels
    labs(x="Day", y="Latitude (°N)", title="Monitoring results") +
    # Legend
    scale_fill_gradientn(name="Proportion\nabove action\nthreshold",
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                         lim=c(0,1)) +
    scale_shape_manual(name="Estimated status", values=c(21, 16)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme

  # Plot managment
  g2 <- ggplot(mgmt_grid_df, aes(x=day, y=lat, fill=status_used, alpha=correct_yn)) +
    geom_raster() +
    # Surveys
    geom_point(data=survey_results, mapping=aes(x=day, y=lat, shape=status_obs), inherit.aes = F, show.legend = F) +
    # Season opener
    geom_vline(xintercept=7, linetype="dotted") +
    # Labels
    labs(x="Day", y="Latitude (°N)", title="Management performance") +
    # Legend
    scale_fill_discrete(name="Management", drop=F) +
    scale_shape_manual(name="Estimated status", values=c(21, 16), drop=F) +
    scale_alpha_manual(name="Action correct?", values=c(0.5, 1), drop=F) +
    # Theme
    theme_bw() + base_theme

  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=1)
  print(g)
  return(g)
}
