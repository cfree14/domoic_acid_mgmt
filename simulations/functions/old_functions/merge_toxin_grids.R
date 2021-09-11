
# Test function
# merge_toxin_grids(grid1=out1a, grid2=out2a)
# merge_toxin_grids(grid1=out1b, grid2=out2b)

# Merge toxin grids
merge_toxin_grids <- function(grid1, grid2, plot=T){

  # Merge grids
  grid_df <- grid1 %>%
    # Merge
    left_join(grid2, by=c("day", "lat")) %>%
    # Finalize proportion
    mutate(prop=pmax(prop.x, prop.y)) %>%
    select(-c(prop.x, prop.y))

  # If plotting
  if(plot){

    # Plot grid
    g <- ggplot(grid_df, aes(x=day, y=lat, fill=prop)) +
      geom_raster() +
      geom_contour(data=grid_df,
                   mapping=aes(x=day, y=lat, z=prop),
                   breaks=c(0.0001, seq(0.1,1,0.1)), color="black", alpha=0.2) +
      # Labels
      labs(x="Day", y="Latitude (°N)") +
      # Legend
      scale_fill_gradientn(name="Proportion above\naction threshhold",
                           na.value = "white",
                           colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,1)) +
      guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
      # Theme
      theme_bw() + theme(legend.position = "bottom")
    print(g)

  }

  # Return
  return(grid_df)

}


